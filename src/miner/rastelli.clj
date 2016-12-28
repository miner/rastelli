(ns miner.rastelli
  (:require [miner.rastelli.pattern :as p]
            [miner.subvec-fix]
            [clojure.spec :as s]
            [clojure.string :as str]))


(def all-balls [:A :B :C :D :E :F :G :H :I :J :K])
(def ball-start (zipmap all-balls (range)))


;; 3 balls: 3, 51, 441, 4413, 531, 711, 5511, 50505
;; 4 balls: 4, 53, 71, 534, 561, 7531, 5551, 55514
;; 5 balls: 5, 64, 73, 97531, 717
;; 7 balls: 867

(def as-pattern p/as-pattern)

(defn siteswap? [pattern]
  (p/siteswap? (as-pattern pattern)))

(defn siteswap
  ([pat] (siteswap pat 0 -1))
  ([pat i] (siteswap pat i (inc i)))
  ([pat i j]
   (p/siteswap (as-pattern pat) i j)))

(defn canonical [pattern]
  (p/canonical (as-pattern pattern)))

(defn orbits [pattern]
  (p/orbits (as-pattern pattern)))

(defn numballs [pattern]
  (p/numballs (p/as-pattern pattern)))

(defn ball-at [timeline beat]
  (when (contains? timeline beat)
    (nth timeline beat)))

;; advance-timeline-by-throw
(defn timeline-throw [timeline beat ball height]
  (if (zero? height)
    timeline
    (let [dest (+ beat height)]
      (when-not ball
        (throw (ex-info "Out of balls" {:throwing nil
                                        :beat beat
                                        :timeline timeline
                                        :height height})))
      (when (ball-at timeline (+ beat height))
        (throw (ex-info "Collision" {:throwing (nth timeline beat)
                                     :beat beat
                                     :timeline timeline
                                     :height height
                                     :already (nth timeline dest)})))
      (if (contains? timeline dest)
        (assoc timeline dest ball)
        timeline))))

(defn calc-timeline [pattern n]
  (let [patt (as-pattern pattern)
        num (numballs patt)
        len (* n num)
        tl (into [] (repeat len nil))
        held (take num all-balls)]
    ;;(println "pattern" patt)
    ;;(println "balls:" held)
    (loop [timeline tl beat 0 held held ps (take len (cycle patt))]
      (if-let [p (first ps)]
        (if (zero? p)
          (recur timeline (inc beat) held (rest ps))
          (let [ball (ball-at timeline beat)
                tl1 (if ball
                      (timeline-throw timeline beat ball p)
                      (timeline-throw (assoc timeline beat (first held)) beat (first held) p))]
            ;;(println "at" beat "throw" (nth tl1 beat) "+" (first ps))
            ;;(println tl1)
            (recur tl1 (inc beat) (if ball held (rest held)) (rest ps))))
        (subvec timeline 0 len)))))

(defn run
  ([patt] (run patt 4))
  ([patt n] (let [patt (as-pattern patt)
                  timeline (calc-timeline patt n)]
              (println "pattern" patt)
              (when-not (siteswap? patt)
                (throw (ex-info "Illegal pattern" {:illegal patt})))
              (doseq [orb (orbits patt)]
                (let [num (numballs orb)]
                  (print "  orbit" orb)
                  (when (> num 1)
                    (print " x" num))
                  (println)))
              (println " " (numballs patt) "balls for" n "cycles")
              (println "[beat Ball R/L  Height]")
              (doseq [x (mapv vector (range) timeline (cycle [:R :L]) (cycle patt))]
                (println x))
              (println)
              timeline)))


(s/def ::siteswap (s/and (s/coll-of (s/int-in 0 10) :kind vector?) p/siteswap?))
