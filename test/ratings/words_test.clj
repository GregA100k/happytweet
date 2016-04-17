(ns ratings.words-test
  (:require [clojure.test :refer :all]
            [equalish.core :as e]
            [ratings.words :as r :refer :all]))

(def testline "10,abide,5.52,1.75,21,3.26,2.22,23,5.33,2.83,18,5,1.87,9,5.92,1.62,12,4.45,2.3,11,2.17,1.53,12,5.6,2.55,10,5,3.3,8,4.29,2.06,7,6.23,1.24,13,4.09,2.3,11,2.5,1.93,12,5.29,3.3,7,5.36,2.66,11,5.38,2.1,13,5.75,1.04,8,2.82,2.04,11,3.67,2.39,12,3.44,2.24,9,7.22,1.99,9")


(deftest build-word-map-test
  (testing "retrieval of the word"
     (is (= "abide" (:word (r/build-word-map testline))))
     (is (= 0.52 (:valence (r/build-word-map testline))))
))


(deftest add-ratings-word-tests
  (testing "adding first word"
    (let [test-map {:words {}}
          word-map (r/build-word-map testline)
          add-result (r/add-ratings-word test-map word-map)
         ]
      ;(is (= true add-result))
      (is (= 1 (count (:words add-result))))
      (is (= add-result {:words {"abide" {:word "abide" :valence 0.52}}}))
  ))
  (testing "adding multiple words"
    (let [test-map {:words {}}
          words-seq [{:word "word1" :valence 5.7} {:word "word2" :valence 3.8}]
          result-map (r/build-up-ratings-map test-map words-seq)
         ]
    (is (= 2 (count (:words result-map))))
    (is (= result-map {:words {"word1" {:word "word1" :valence 5.7} "word2" {:word "word2" :valence 3.8}}}))
  ))
)

(deftest read-in-ratings
  (testing "reading word map from file"
    (let [filename "resources/data/Warriner_sample.csv"
          themap (r/read-ratings-file filename)
         ]
      (is (not (nil? themap)))
    ))
)

(deftest get-the-valence
  (testing "word in the list"
    (let [test-map {:words {}}
          word-map (r/build-word-map testline)
          result (r/add-ratings-word test-map word-map)
          x (r/reset-word-db result)
         ]
      (is (= 0.52 (r/get-valence "abide")))
    ))
  (testing "word not in the list"
    (let [test-map {:words {}}
          word-map (r/build-word-map testline)
          result (r/add-ratings-word test-map word-map)
          x (r/reset-word-db result)
         ]
      (is (= 0 (r/get-valence "notInList")))
    ))
)

(deftest valence-of-message
  (testing "multiple words"
    (let [filename "resources/data/Warriner_sample.csv"
          db (r/read-ratings-file filename)
          message "abandon the aardvark"
            ;; abandon 2.84    -2.16
            ;; aardvark 6.26   1.26
         ]
      (is (e/=ish -0.90 (r/get-message-valence message)))
    ))
)
