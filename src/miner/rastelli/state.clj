(ns miner.rastelli.state
  (:require [clojure.set :as set]
            #_ [clojure.data.int-map :as i])
  )

;; NOTE: some of this is used only by miner.rastelli.loom

;; Definitely prefer state in long, bit order=time.  Better for making graphs with simple
;; state as nodes.

(set! *unchecked-math* :warn-on-unboxed)

;; state could be vector of landings of size max throw
;; or just implied zeros at higher indices

;; shift could be done with  (subvec v 1))
;; but subvec will keep original vector around, additing to it in effect, can't be gc-ed

;; should check that first state 0 implies only a 0 throw

;; Note: bit order = time.  Bit 0 is happening next, Bit 1 at now+1, etc.  But in diagrams,
;; we normally show it is a vector of bits (starting at index 0) which is the opposite order
;; of bits for numbers.
;;
;; Typically have a small number of state bits so it's fine to use a vector of bits, but for
;; long term storage in makes sense to convert back to a long.  (Still thinking about this!)


;; Considering various different state representations:
;;   vector of 1/0
;;   vector of true/false -- seems natural for Clojure
;;   long as a bitmask, ordered bit=time (seemingly backwards in diagrams)


;; FOR NOW: vector true/false for landing at time = now+index


;; Note: width of a state should be same as max allowed throw for transitions
;; 0s are implied for any bits beyond the stated number

(defn init-state [max-throw]
  (vec (repeat max-throw false)))

(defn safe? [state th]
  (and (<= th (count state))
       (not (get state th))))

;; unchecked for crashes
(defn exec [state th]
  (subvec (assoc state (count state) false th true) 1))

(defn transition
  ([state th]
   (when (safe? state th)
     (exec state th)))
  ([state th th2]
   (transition (transition state th) th2))
  ([state th th2 th3]
   (transition (transition state th th2) th3))
  ([state th th2 th3 & more]
   (reduce transition (transition state th th2 th3) more)))
  


;; Maybe dangerous to stack up too many subvecs?  Not sure about holding onto to much
;; garbage.
;;
;; Could use pop vectors or queues?




;; much faster than others
(defn state-mask [lsb-vec]
  (reduce-kv (fn [n i b] (if b (bit-set n i) n)) 0 lsb-vec))

(defn as-lsb
  ([mask] (as-lsb 0 mask))
  ([width mask]
   (mapv #(bit-test mask %) (range (max 1 width (- 64 (Long/numberOfLeadingZeros mask)))))))

(defn lsb-str
  ([mask-or-vec] (lsb-str 0 mask-or-vec))
  ([width mask-or-vec]
   (apply str (map (fn [b] (if b "1" "0")) (if (int? mask-or-vec)
                                             (as-lsb width mask-or-vec)
                                             mask-or-vec)))))



(defn binit [num-balls]
  (reduce bit-set 0 (range num-balls)))

;; Not sure about th=0

;; Issue: if landing 0 = false, there's no old ball to throw.  Is it safe to assume you can get
;; a ball? (NO)  Or should we not allow any throw when t0=0?  Decided: only 0 throw if t0=0.

;; bstate is a bitmask treated in bit=time order, always shifting right
(defn btrans
  ([bstate th]
   (if (bit-test bstate 0)
     (when-not (bit-test bstate th)
       (bit-shift-right (bit-set bstate th) 1))
     (when (zero? th)
       (bit-shift-right bstate 1))))
  ([bstate th th2]
   (btrans (btrans bstate th) th2))
  ([bstate th th2 th3]
   (btrans (btrans (btrans bstate th) th2) th3))
  ([bstate th th2 th3 & more]
   (apply btrans (btrans bstate th th2 th3)
          more)))







;; reworking for Loom and Ubergraph notation of adjacency graph
;; node names first, val is "weight" which we will use as the throw label
;; Basically, just swapping the key/val in the submaps

(defn successor-bstate-throws [bstate max-throw]
  (if (bit-test bstate 0)
    ;; there's a ball
    (reduce (fn [m th]
              ;; conflict if the ball would land in an occupied slot
              (if (bit-test bstate th)
                m
                (assoc m (bit-shift-right (bit-set bstate th) 1) th)))
            {}
            (range (inc max-throw)))
    ;; no ball, so only zero works
    {(bit-shift-right bstate 1) 0}))


;; Needed for Loom/Ubergraph approach in loom.clj
;; SEM FIXME: move it to loom.clj with other wadj- stuff
;; and rename to graph.clj

;; returns {st1 {st2 th2 st3 th3 ...} st2 {...} ...}
(defn wadj-map [num-balls max-throw]
  (loop [wadj {} states (list (binit num-balls))]
    (cond (empty? states) (with-meta wadj {::ballCount num-balls ::max-throw max-throw})
          (contains? wadj (first states)) (recur wadj (rest states))
          :else (let [st1 (first states)
                      trans1 (successor-bstate-throws st1 max-throw)]
                  (recur (assoc wadj st1 trans1) (into (rest states) (keys trans1)))))))



;; bitCount is always the number of balls at binit
;; width is always the highest throw allowed at binit
;; We can recover that, but it seems expensive.
(defn wadj-width [wadj]
  (- 64 (Long/numberOfLeadingZeros (apply max (keys wadj)))))

(defn wadj-balls [wadj]
  (Long/bitCount (ffirst wadj)))

;; IDEA:  put the numballs and width on the meta for the wadj

;; could just store the states and calc throws when needed
(defn calc-throw [bstate0 bstate1]
  (if (bit-test bstate0 0)
    (let [x (bit-xor (bit-shift-right bstate0 1) bstate1)]
      (when (= (Long/bitCount x) 1)
        (inc (Long/numberOfTrailingZeros x))))
    (when (= (bit-shift-right bstate0 1) bstate1)
      0)))

(defn mapk [fk mp] 
  (persistent! (reduce-kv (fn [m k v] (assoc! m (fk k) v)) (transient {}) mp)))

(defn mapmap
  [fk fv mp]
  (persistent!
   (reduce-kv (fn [tm k v] (assoc! tm (fk k) (fv v)))
              (transient {})
              mp)))

(defn lsb-wadj [numballs maxthrow]
  (let [kf #(lsb-str maxthrow %)]
    (mapmap kf #(mapk kf %) (wadj-map numballs maxthrow))))

    


(defn stateseq->pattern [states]
  (when (seq states)
    (let [edges (partition 2 1 states)]
      (if (seq edges)
        (mapv #(apply calc-throw %) edges)
        (when-let [base (calc-throw (first states) (first states))]
          (vector base))))))



;; SEM NEW IDEA use int-map and int-set since the states and throws are encoded as ints
;; UNIMPLEMENTED

;; BETTER IDEA -- find any cycle, not just base
;; when overlap with path, add it


(defn successor-bstates [bstate max-throw]
  (if (bit-test bstate 0)
    ;; there's a ball
    (reduce (fn [bset th]
              ;; conflict if the ball would land in an occupied slot
              (if (bit-test bstate th)
                bset
                (conj bset (bit-shift-right (bit-set bstate th) 1))))
            ()
            (range (inc max-throw)))
    ;; no ball, so only zero works
    (list (bit-shift-right bstate 1))))

;; similar to wadj-map but doesn't store throws, just the successor states as list
;; #{Set} values didn't seem to be worth it.
(defn successor-map [num-balls max-throw]
  (loop [ss {} states #{(binit num-balls)}]
    (if (empty? states)
      ss 
      (let [st1 (first states)
            trans1 (successor-bstates st1 max-throw)
            ss2 (assoc ss st1 trans1)]
        (recur ss2 (into (disj states st1) (remove ss2 trans1)))))))

;; note (some #{n} coll) is slower
;; than (some #(= n %) coll)
;; for integer n, and relatively small coll (100 elements)
;; probably hashing is more expensive than =
;; doesn't appear to be the case for kw

;; For our purposes, a circuit is any path the goes back to a state already in the path.
;; At first, we only cared about simple circuits that returned to the original state, but
;; we relaxed that constraint to any state already on the path.

(defn find-circuits [numballs maxthrow]
  (let [base (binit numballs)
        ss (successor-map numballs maxthrow)]
    (loop [paths (map #(conj [base] %) (get ss base)) circuits []]
      (if (seq paths)
        (let [path (first paths)
              end (peek path)]
          (if (some #(= end %) (pop path))
            (recur (rest paths) (conj circuits path))
            (recur (into (rest paths) (map #(conj path %) (get ss end))) circuits)))
        circuits))))




;; Didn't use the general algorithms, because we have special knowlege that graph is
;; strongly connected (by construction, I think, but really should check), and we know the
;; base state (lowest value) because is the standard n-ball pattern, or ground-state.
