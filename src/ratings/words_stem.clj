(ns ratings.words-stem
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.numeric-tower :as math]
            [clj-tokenizer.core :as token]
))


(def word-db "atom containing a map of with a single key, for now,
  of :words.  The value for :words is another map with the string
  of the stem of a word for the key and a map of containing 
  :stem, :word and :valence"
  (atom {:words {}}))

(defn- center 
  "Rather than having a neutral value of 5, center neutral,
   neither positive or negative, at 0"
  [n]
  (/ (math/round (- (* 100 n) 500)) 100.0))
 
(defn get-stem [w]
  (first (token/token-seq (token/stemmed (token/token-stream w)))))

(defn build-word-map 
  "given line of text with comma separated numbers
   as from the Ratings_Warriner_et_al.csv file, convert 
   that string into a map with the following keys
    ,Word,V.Mean.Sum,V.SD.Sum,V.Rat.Sum,A.Mean.Sum,A.SD.Sum,A.Rat.Sum,D.Mean.Sum,D.SD.Sum,D.Rat.Sum,V.Mean.M,V.SD.M,V.Rat.M,V.Mean.F,V.SD.F,V.Rat.F,A.Mean.M,A.SD.M,A.Rat.M,A.Mean.F,A.SD.F,A.Rat.F,D.Mean.M,D.SD.M,D.Rat.M,D.Mean.F,D.SD.F,D.Rat.F,V.Mean.Y,V.SD.Y,V.Rat.Y,V.Mean.O,V.SD.O,V.Rat.O,A.Mean.Y,A.SD.Y,A.Rat.Y,A.Mean.O,A.SD.O,A.Rat.O,D.Mean.Y,D.SD.Y,D.Rat.Y,D.Mean.O,D.SD.O,D.Rat.O,V.Mean.L,V.SD.L,V.Rat.L,V.Mean.H,V.SD.H,V.Rat.H,A.Mean.L,A.SD.L,A.Rat.L,A.Mean.H,A.SD.H,A.Rat.H,D.Mean.L,D.SD.L,D.Rat.L,D.Mean.H,D.SD.H,D.Rat.H

  Since I have not read the whole paper, I will only include
  :word, :valence (V.Mean.Sum), :arousal (A.Mean.Sum), :dominance (D.Mean.Sum)
  "
  [r]
  (let [parts (vec (str/split r #","))
        the-word (get parts 1)
       ]
    {:word the-word 
     :valence (center (read-string (get parts 2)))
     :stem (get-stem the-word)
    }
))

(defn add-ratings-word 
  "add a new wm - word-map to the larger map rm - ratings map
   Within rm, the key :words points to a map keyed by the individual 
   word lemmas.  The value of that map is a map of the stats for
   that given word"
  [rm wm]
  (assoc-in rm [:words (:stem wm)] wm)
)

(defn build-up-ratings-map 
  [m s]
  (reduce (fn [im v] (add-ratings-word im v)) m s))


(defn- read-ratings-file-rows [filename]
  (with-open [rdr (io/reader filename)]
    (doall (map build-word-map (rest (line-seq rdr))))))

(defn read-ratings-file [filename]
  (build-up-ratings-map {:words {}} (read-ratings-file-rows filename)))

(defn reset-word-db [wdb]
  (reset! word-db wdb))


(defn get-valence [w]
  (let [stemword (get-stem w)]
  (get-in @word-db [:words stemword :valence] 0)))

(defn get-message-valence 
  "Simple split of the message splitting on spaces only.
   Each word in the message is looked up in the word-db to get
   it's valence value.  The sum of the valence values is the 
   score for the message"
  [m]
  
  (let [parts (str/split m #" ")
       ]
    (reduce + (map #(get-valence %) parts))))

(defn get-message-words-scored
  "count of how many words actually got scores"
 [m]
  (let [parts (str/split m #" ")
        valence-exists (fn [w] (get-in @word-db [:words (get-stem w) :valence]))
        add-existence (fn [v w] [(if (valence-exists w) (inc (first v)) (first v)) (inc (second v))])
       ]
    (reduce add-existence [0 0] parts)))

(reset-word-db (read-ratings-file "resources/data/Ratings_Warriner_et_al.csv"))

