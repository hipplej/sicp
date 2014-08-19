(ns sicp.chapter-4
  (:require [clojure.repl :refer :all]))

;; Exercise 4.21
;; Recursion with no names!
((fn [n]
   ((fn [fact]
      (fact fact n))
    (fn [ft k]
      (if (= k 1)
        1
        (* k (ft ft (- k 1)))))))
 10)

(defn f [x]
  ((fn [even? odd?] ;; 2 parameters, first is even function, second is odd function
     (even? even? odd? x))
   (fn [ev? od? n] ;; First parameter, even function
     (if (= n 0) true (od? ev? od? (- n 1))))
   (fn [ev? od? n] ;; Second parameter, odd function
     (if (= n 0) false (ev? ev? od? (- n 1))))))
