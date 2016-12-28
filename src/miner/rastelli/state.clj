(ns miner.rastelli.state

  )


;; state could be vector of landings of size max throw
;; or just implied zeros at higher indices

;; shift could be done with  (subvec v 1))
;; but subvec will keep original vector around, additing to it in effect, can't be gc-ed

;; should check that first state 0 implies only a 0 throw




;; Considering two different state representations:
;;   vector of 1/0
;;   long as a bitmask, ordered lsb-first

;; Note: width of a state should be same as max allowed throw for transitions
;; 0s are implied for any bits beyond the stated number

(defn transition [state th]
  (subvec (assoc state (count state) 0 th 1) 1))

;; bstate is a bitmask treated in lsb order, always shifting right
(defn btrans
  ([bstate th]
   (bit-shift-right (bit-set bstate th) 1))
  ([bstate th th2]
   (btrans (btrans bstate th) th2))
  ([bstate th th2 th3]
   (btrans (btrans (btrans bstate th) th2) th3))
  ([bstate th th2 th3 & more]
   (apply btrans (btrans bstate th th2 th3)
          more)))
  

;; least-significant bit first
(defn lsb-vec
  "Returns sequence of 1s or 0s from (long) mask, least significant bit first.  Optional
  width limits the number of bits.  Default width elides trailing 0s."
  ([mask] (lsb-vec 0 mask))
  ([width mask]
   (let [width (if (zero? width)
                 (max 1 (- 64 (Long/numberOfLeadingZeros mask)))
                 width)]
     (mapv (fn [i] (if (bit-test mask i) 1 0)) (range width)))))

(defn lsb-str
  ([mask] (lsb-str 0 mask))
  ([width mask]
   (apply str (lsb-vec width mask))))

(defn bitmask [lsb-vec]
  "Returns the long integer corresponding to the given 1 or 0 'bits' interpreted with the
  least-significant bit first.  The result is undefined for a collection larger than 64
  bits, but the limit is not checked."
  (transduce (keep-indexed (fn [i b] (when (= b 1) i))) (completing bit-set) 0  lsb-vec))



