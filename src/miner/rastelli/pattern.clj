(ns miner.rastelli.pattern
  (:require [miner.rastelli.util :as util]
            miner.subvec-fix
            [clojure.spec :as s]) )

;; convenience for converting other notations in vector of ints
(defn as-pattern [pat]
  ;; seq ints, str of digits, single int, nil
  ;; always returns a vector of ints
  (cond
      (vector? pat) pat
      (coll? pat) (vec pat)
      (nil? pat) []
      (nat-int? pat) (mapv util/ch->digit (str pat))
      (and (string? pat) (re-matches #"[0-9 ]+" pat)) (mapv util/ch->digit (remove #{\space} pat))
      :else (throw (ex-info "Invalid pattern" {:invalid pat}))))

(defn pattern-str [pat]
  (cond
      (nil? pat) "[]"
      (coll? pat) (apply str pat)
      :else (str pat)))

(defn- dupn [pat n]
  ;; check for N number of repeating subpatterns
  (when (util/zmod? (count pat) n)
    (let [d (quot (count pat) n)
          parts (partition d pat)]
      (when (apply = parts)
        (vec (first parts))))))

;; SEM normally the highest number would come first
;; consider rotations and canonical form
;; http://juggle.wikia.com/wiki/Siteswap

;; maybe should be used by canonical
(defn dedupe-pattern [pat]
  (or (first (keep #(dupn pat %) (range (count pat) 1 -1))) pat))


;; The issue with reduce-kv not working on a subvector is fixed by the protocol extension in
;; miner.subvec-fix.  See CLJ-2065.

(defn pattern-rotations
  "Returns seq of rotations of original vector pattern."
  [pat]
  (let [pp (into pat pat)
        cnt (count pat)
        offsets (if (odd? cnt)
                  (range cnt)
                  (range 0 cnt 2))]
    (map #(subvec pp % (+ % cnt)) offsets)))

;; Even-period patterns start on one side, but don't have symmetric opposite.
;; Similar in mirror image, but we do not consider equal.  Use isomorphic? to accept mirror
;; image.

(defn symmetric? [pat]
  (let [cnt (count pat)]
    (every? #(= (pat %) (pat (- (dec cnt) %))) (range (quot cnt 2)))))

;; prefer symmetric patterns, then by higher throws first
(defn canonical [pat]
  (let [pats (pattern-rotations pat)
        syms (filter symmetric? pats)]
    (reduce (fn [res x] (if (neg? (compare res x)) x res)) (or (seq syms) pats))))

;; pat must be vector
(defn- vrot1 [pat]
  (if (empty? pat)
    pat
    (conj (subvec pat 1) (pat 0))))

;; true if canonically same or mirror images
(defn isomorphic? [pat1 pat2]
  (let [can1 (canonical pat1)
        can2 (canonical pat2)]
    (or (= can1 can2)
        (when (even? (count pat1))
          (let [can11 (canonical (vrot1 can1))]
            (= can11 can2))))))
            

;; want them all to land at different times
(defn siteswap? [pattern]
  (let [cnt (count pattern)]
    (or (zero? cnt)
        (apply distinct? (map #(mod % cnt) (map-indexed + pattern))))))

;; patterns are typically just a few items so no need to use transducers

  
;; Note: numballs of course also works for orbits
(defn numballs ^long [pattern]
  (let [cnt (count pattern)]
    (if (zero? cnt)
      0
      (let [n (/ (reduce + pattern) (count pattern))]
        (when-not (integer? n)
          (throw (ex-info "Illegal pattern" {:pattern pattern :ball-count n})))
        n))))


(defn- toss [pattern beat]
  (nth pattern (mod beat (count pattern))))

(defn- hand [beat]
  (if (odd? beat) :L :R))

(defn- htoss [pattern beat]
  [(hand beat) (toss pattern beat)])

(defn- next-beat [pattern beat]
  (+ beat (toss pattern beat)))

(defn- next-pos ^long [pattern beat]
  (mod (next-beat pattern beat) (count pattern)))

(defn- pset->orbit [pat pset]
  (let [zorbit (vec (repeat (count pat) 0))
        orbit  (reduce (fn [res i] (assoc res i (nth pat i))) zorbit pset)]
    (when-not (= zorbit orbit)
      orbit)))

;; SEM: could be less ugly, maybe with more set/fns
(defn- zorbs [pat]
    (when (siteswap? pat)
      (loop [index 0 ps (set (range 1 (count pat))) orb #{} orbs []]
        ;;(println "zorbs " index ps orb)
        (if (contains? orb index)
          (if (empty? ps)
            (conj orbs orb)
            (let [start (long (reduce min ps))]
              (recur start (disj ps start) #{} (conj orbs orb))))
          (recur (next-pos pat index) (disj ps index) (conj orb index) orbs)))))

;; SEM -- need to study orbits more
;; guess odd number or balls will have symmetric orbits
;; 97531 -- 5 balls -- 91 and 19, 73 and 37, 5
;; but what's the difference between multiple balls in exact same orbit and its reflection?
;; Should reflection be listed as a separate orbit?
;; could try doubling odd pattern and running zorts
;; need to minimize multi-repeat throw [3 3 3] = [3], [5 5] = [5]
;; Wonder if orbits should be listed with blanks or 0s so that they can compose correctly??? YES
;;   But [0 0 0] orbit is degenerate and shouldn't be reported.

;; orbits should canoncialize and merge equivalents:  [3 0 0] = [0 0 3]

;; NO

;; SEM should assign balls to orbits

;; canonical distinguishes left/right start 17 vs 71
(defn orbits [pat]
  (let [pat (dedupe-pattern pat)]
    (dedupe (sort (comp - compare)
                  (sequence (comp (map #(pset->orbit pat %)) (remove nil?))
                            (zorbs pat))))))


  
;; returns list of starting beats
(defn ball-starts [pattern]
  (let [cnt (numballs pattern)
        cnt2 (* 2 cnt)]
    (loop [pat2 (vec (repeat cnt2 nil)) n cnt i 0]
      (cond
          (zero? n)
          (vals (reduce-kv (fn [res i b] (if (or (not b) (res b)) res (assoc res b i))) {} pat2))

          (>= i cnt2)
          (let [hole (util/nil-index pat2)]
            (if (nil? hole)
              (recur pat2 0 0)
              (recur pat2 (dec n) (long hole))))

          ;; force another cycle for a 0 throw
          (zero? (util/modth pattern i))
          (recur (assoc pat2 i false) (inc n) cnt2)

          :else (recur (assoc pat2 i n) n (+ i (long (util/modth pattern i))))))))


(defn siteswap [pat i j]
  (let [cnt (count pat)
        j (mod j cnt)]
    (cond
        (>= i cnt) nil
        (= i j) (update pat i + (count pat))
        (< i j) (let [d (- j i)
                      vi2 (+ (pat j) d)
                      vj2 (- (pat i) d)]
                  (when-not (neg? vj2)
                    (assoc pat i vi2 j vj2))))))


