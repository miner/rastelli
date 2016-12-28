(ns miner.test-rastelli
  (:require [clojure.test :refer :all]
            [miner.rastelli :refer :all]))

(deftest juggling-test
  (testing "basic juggling"
    (is (= (numballs (as-pattern 3)) 3))))

(deftest test-orbits
  (testing "orbits juggling"
    (is (= (set (orbits 97531))
           (set [[9 0 0 0 1] [0 7 0 3 0] [0 0 5 0 0]])))))

(deftest test-siteswaps
  (is (every? siteswap? (range 10)))
  (is (every? siteswap? (map str (range 10))))
  (is (every? siteswap? (map vector (range 10))))

  ;; 3 balls:
  (is (every? siteswap? [3, 51, 441, 4413, 531, 711, 5511, 50505]))
  (is (every? #{3} (map numballs [3, 51, 441, 4413, 531, 711, 5511, 50505])))

  ;; 4 balls:
  (is (every? siteswap? [4, 53, 71, 534, 561, 7531, 5551, 55514]))
  (is (every? #{4} (map numballs [4, 53, 71, 534, 561, 7531, 5551, 55514])))
  
  ;; 5 balls:
  (is (every? siteswap? [5, 64, 73, 97531, 717]))
  (is (every? #{5} (map numballs [5, 64, 73, 97531, 717])))
  
  ;; 7 balls: 867
  (is (every? siteswap? [867]))
  (is (every? #{7} (map numballs [867])))

  (is (not-any? siteswap? [[5 4 3] [1 2 1] [5 1 2]])))



