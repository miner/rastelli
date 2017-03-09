(ns miner.subvec-fix)

;; Work-around for CLJ-2065

;; Clojure bug:
;; (reduce-kv + 0 [2 4 5 6])   ;;=> 23  OK
;; (reduce-kv + 0 (subvec [2 4 5 6] 1))  Error:
;; IllegalArgumentException No implementation of method: :kv-reduce of protocol:
;;  #'clojure.core.protocols/IKVReduce found for class:
;;  clojure.lang.APersistentVector$SubVector

;; work-around for subvector missing protocol support

(when-not (satisfies?   clojure.core.protocols/IKVReduce (subvec [1] 0))
  (extend-type clojure.lang.APersistentVector$SubVector
    clojure.core.protocols/IKVReduce
    (kv-reduce
      [subv f init]
      (let [cnt (.count subv)]
        (loop [k 0 ret init]
          (if (< k cnt)
            (let [val (.nth subv k)
                  ret (f ret k val)]
              (if (reduced? ret)
                @ret
                (recur (inc k) ret)))
            ret))))))
