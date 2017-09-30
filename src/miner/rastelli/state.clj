(ns miner.rastelli.state
  (:require [loom.graph :as lg]
            [loom.alg :as la]
            [loom.io :as lio]) )

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
;; a ball?  Or should we not allow any throw when t0=0?

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







;; bad name
(defn btrans1 [bstate max-throw]
  (reduce (fn [m th] (if-let [newst (btrans bstate th)] (assoc m th newst) m)) {}
          (range (inc max-throw))))

(defn bgraph [num-balls max-throw]
  (loop [graph {} states (list (binit num-balls))]
    (cond (empty? states) graph
          (contains? graph (first states)) (recur graph (rest states))
          :else (let [st1 (first states)
                      trans1 (btrans1 st1 max-throw)]
                  (recur (assoc graph st1 trans1) (into (rest states) (vals trans1)))))))




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


(defn view [num-balls max-throw]
  (let [wadj (lsb-wadj num-balls max-throw)
        g (lg/weighted-digraph wadj)]
    (lio/view g)))
    


(defn states->pattern [states]
  (when (seq states)
    (let [edges (partition 2 1 states)]
      (if (seq edges)
        (map #(apply calc-throw %) edges)
        (list (calc-throw (first states) (first states)))))))

;; UNIMPLEMENTED -- use the graph stuff instead
(defn find-patterns
  ([graph] (find-patterns graph 5))
  ([graph max-length]
   (let [base (apply min (lg/nodes graph))
         starts (lg/successors graph base)]
     (map #(states->pattern (cons base (la/bf-path graph % base))) starts))))


;; assumes we only care about starting from base (lowest state)
;; will not find multiple cycle circuits
;; simple circuits never visit the same node twice (except for the start)
(defn find-simple-circuits [graph]
  (let [base (apply min (lg/nodes graph))]
    (loop [paths (list [base]) circuits []]
      (if (seq paths)
        (let [path (first paths)
              succs (lg/successors graph (peek path))
              found? (some #{base} succs)
              ;; remove any successors already in the path, avoiding loops
              ;; note: base is always the start of path
              exts (remove (set path) succs)]
          ;;(println paths circuits found? exts)
          (recur (into (rest paths) (map #(conj path %) exts))
                 (if found? (conj circuits (conj path base)) circuits)))
        circuits))))

;; only slightly faster if we keep the path-set around, not worth the extra code



(defn find-patterns-the-hard-way [numballs maxthrow]
  (let [graph (lg/weighted-digraph (wadj-map numballs maxthrow))]
    (map states->pattern (find-simple-circuits graph))))


;; Didn't use the general algorithms, because we have special knowlege that graph is
;; strongly connected (by construction, I think, but really should check), and we know the
;; base state (lowest value) because is the standard n-ball pattern, or ground-state.

;; probably need other search alg for cycles


;; https://blog.mister-muffin.de/2012/07/04/enumerating-elementary-circuits-of-a-directed_graph/
;; algos: Tarjan, Johnson, and Hawick

;; Lots of potential here, in Java:
;; https://github.com/jgrapht/jgrapht

;; need transitive closure
        
;; Much more compact and probably better for hashing as ints
; {st1 {th1 st11 th2 st12}
; st11 {th2 st112 th3 st113}
; ...}


(comment

  (def g35 (lg/weighted-digraph (wadj-map 3 5)))

  )
