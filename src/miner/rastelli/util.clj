(ns miner.rastelli.util)

(defn zmod? [n d]
  (zero? (rem n d)))

;; v vector, n nat-int
(defn modth [v n]
  (nth v (mod n (count v))))

;; char 0-9
(defn ch->digit [chr]
  (- (long chr) (long \0)))

(defn first-index [f coll]
  (first (keep-indexed (fn [i x] (when (f x) i)) coll)))

(defn nil-index [v]
  (first-index nil? v))
