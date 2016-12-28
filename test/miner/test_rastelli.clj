(ns miner.test-rastelli
  (:require [clojure.test :refer :all]
            [miner.rastelli :refer :all]))

(deftest juggling-test
  (testing "basic juggling"
    (is (= (numballs (as-pattern 3)) 3))))

