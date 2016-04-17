(ns equalish.core
  (:require [clojure.math.numeric-tower :as math]))


(defn =ish 
  "If a and b are numbers test to see if they are within a small
   tolerance.  If they are not numbers just use ="
  [a b]
  (if (and (number? a) (number? b))
     (> 0.00001 (math/abs (- a b)))
     (= a b)))

