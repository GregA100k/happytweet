(ns ratings.runner-test
  (:require [clojure.test :refer :all]
            [ratings.runner :as r :refer :all]))

(def t1 {:id "14076", :score 0, :text "omitted...", :valence -0.96})


(deftest right-or-wrong-test
  (testing "neutral score"
    (is (= false (r/right-or-wrong {:score 2 :valence 1.3})))
    (is (= true (r/right-or-wrong {:score 2 :valence 0}))))
  (testing "negative score"
    (is (= true (r/right-or-wrong t1)))
    (is (= false (r/right-or-wrong {:score 0 :valence 0}))))
  (testing "positive score"
    (is (= false (r/right-or-wrong {:score 4 :valence 0})))
    (is (= false (r/right-or-wrong {:score 4 :valence -1.2})))
    (is (= true (r/right-or-wrong {:score 4 :valence 2.0}))))
)

(deftest build-correctness-stats
  (testing "initial tweet"
    (is (= {:negative {:correct 1 :total 1}} (r/compile-scores {} t1)))
  )
)
