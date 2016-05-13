(ns ratings.words-stem-test
  (:require [clojure.test :refer :all]
            [equalish.core :as e]
            [ratings.words-stem :as r :refer :all]))

(def testline "10,abide,5.52,1.75,21,3.26,2.22,23,5.33,2.83,18,5,1.87,9,5.92,1.62,12,4.45,2.3,11,2.17,1.53,12,5.6,2.55,10,5,3.3,8,4.29,2.06,7,6.23,1.24,13,4.09,2.3,11,2.5,1.93,12,5.29,3.3,7,5.36,2.66,11,5.38,2.1,13,5.75,1.04,8,2.82,2.04,11,3.67,2.39,12,3.44,2.24,9,7.22,1.99,9")

(def testline "10548,run,6.41,1.4,22,5.24,2.68,21,5.44,1.79,18,7,1.55,6,6.2,1.37,15,5.71,2.69,7,5,2.75,14,4.67,1.58,9,6.38,1.77,8,6.5,1.51,14,6.29,1.38,7,4.33,3.04,9,5.92,2.27,12,5.6,2.12,10,5.25,1.39,8,6.64,1.55,14,6,1.07,8,5.33,3.5,6,5.2,2.43,15,5.7,2.16,10,5.12,1.25,8")



(deftest perform-stemming
  (testing "simple stem call"
      (is (= "run" (r/get-stem "running")))
    )
)

(deftest stem-valence
  (testing "plain word"
    (let [test-map {:words {}}
          word-map (r/build-word-map testline)
          ;t (println word-map)
          result (r/add-ratings-word test-map word-map)
          ;t2 (println result)
          x (r/reset-word-db result)
         ]
    (is (= 1.41 (r/get-valence "run")))
    (is (= 1.41 (r/get-valence "running"))))
))
