(ns ratings.runner
  (:require [ratings.words :as words]
            [ratings.words-stem :as wordstem]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as p]
            [clojure.math.numeric-tower :as math])
)


(def tweet-file "resources/data/testdata.manual.2009.06.14.csv")

(defn- parse-tweet-row 
  "The tweet file is made up of comma separated strings.  The values have
   double quotes around them and occur in the following order:
   :score, :id, date and time (ignored), ignored, ignored, :text

   a map of those keys and the values contained in the row is returned"
  [r]
  (let [parts (str/split r #",")]
    {:id (get parts 1) :score (read-string (read-string (get parts 0))) :text (get parts 5)}
))

(defn- build-tweet-list [tr-seq]
  (map parse-tweet-row tr-seq)
)

(defn read-tweet-file [filename]
  (with-open [rdr (io/reader filename)]
    (doall (build-tweet-list (line-seq rdr)))))

(defn process-tweet [t f]
  (let [valence (f (:text t))
       ]
    (assoc t :valence valence)
))

(defn right-or-wrong [{score :score valence :valence}]
  (cond
    (= 0 score) (> 0 valence)
    (= 2 score) (= 0 valence)
    (= 4 score) (< 0 valence)
))

(defn compile-scores
  "very procedural function to take a map of score compilattions
   and add in the score from this tweet.  The tweet will already 
   have a score of 0, 2, or 4 which gets translated to a keyword 
   label.  The tweet will also have a valence value which is
   compared to the score to see if it is :correct or :incorrect.
   Both the :correct/:incorrect count and the :total count are
   incremented"
 [m t]
  (let [calc-label (fn [{score :score}]
                      (cond 
                        (= 0 score) :negative
                        (= 2 score) :neutral
                        (= 4 score) :positive
                        :else :unknown))

        label (calc-label t)
        score (right-or-wrong t)
        calc-right-label (fn [s] (if s :correct :incorrect))
        right-label (calc-right-label score)
        labels-to-inc [right-label :total]
        temp2  (reduce #(update-in %1 [label %2] (fnil inc 0)) m labels-to-inc)
       ]
    temp2
))

(defn- add-percent-correct
  "given a score map containing values for number :correct
   and the :total tweets tested, compute the :percent-correct
   and add it to the map"
  [sm] (assoc sm :percent-correct (/ (get sm :correct 0) (:total sm) 1.0)))

(defn percent-correct [s]
   (reduce #(assoc-in %1 [%2] (add-percent-correct (%2 %1))) s (keys s)))

(defn overall-percents [m]
  (let [startamounts {:correct 0, :incorrect 0, :total 0}
        accum-totals (fn [t [k v]] (if (= k :percent-correct) t
                                   (update-in t [k] + v)))
        totalamounts (reduce (fn [t i] (reduce accum-totals t i)) startamounts (vals m))
       ]
      (add-percent-correct totalamounts)))


(defn -main []
  (def tweet-list (read-tweet-file tweet-file))
  (def scores 
    (doall (map #(process-tweet % words/get-message-valence) tweet-list)))
  (def compiled-scores (reduce compile-scores {} scores))
  (p/pprint compiled-scores)
  (println)
  (def compiled-percents (percent-correct compiled-scores))
  (p/pprint compiled-percents)
  (println "-- overal totals --")
  (p/pprint (overall-percents compiled-percents))
  (println)
  (println "totals of words scored " 
    (apply map + 
      (map #(words/get-message-words-scored (:text %)) tweet-list)))
  (println)


  (println "-- totals for stemmed words -- ")
  (def stem-scores 
    (doall (map #(process-tweet % wordstem/get-message-valence) tweet-list)))
  (def compiled-stem-scores (reduce compile-scores {} stem-scores))
  (p/pprint compiled-stem-scores)
  (println)
  (def compiled-stem-percents (percent-correct compiled-stem-scores))
  (p/pprint compiled-stem-percents)
  (println "-- overal totals --")
  (p/pprint (overall-percents compiled-stem-percents))
  (println)
  (println "totals of words scored " 
    (apply map + 
      (map #(wordstem/get-message-words-scored (:text %)) tweet-list)))
)

