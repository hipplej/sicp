(ns sicp.chapter-2
  (:require [clojure.math.numeric-tower :as math]))

;; -----------------------------------------------------------------------------
;; Exercise 2.4

;; Returns a function that expects a single argument that is a function that takes two arguments.
;; The two arguments that get passed to that function are the two arguments originally passed to my-cons.
(defn my-cons [x y]
  (fn [m] (m x y)))

;; Takes a single argument that is a function that returns a function that expects a single
;; argument that is a function that expects two arguments. The original argument is called,
;; passing a function function that returns the second of its two arguments.
(defn my-car [z]
  (z (fn [p q] p)))

;; Takes a single argument that is a function that returns a function that expects a single
;; argument that is a function that expects two arguments. The original argument is called,
;; passing a function function that returns the second of its two arguments.
(defn my-cdr [z]
  (z (fn [p q] q)))

;; -----------------------------------------------------------------------------
;; Exercise 2.5
(defn exp-cons [a b]
  (* (math/expt 2 a) (math/expt 3 b)))

;; Returns the number of times n can be successively divided by base with no remainder.
(defn num-divisions-until-remainder [n base]
  (letfn [(div-iter [x times]
            (if (not (zero? (rem x base))) times
                (recur (/ x base) (inc times))))]
    (div-iter n 0)))

(defn exp-car [p]
  (num-divisions-until-remainder p 2))

(defn exp-cdr [p]
  (num-divisions-until-remainder p 3))

;; -----------------------------------------------------------------------------
;; Exercise 2.17
(defn last-pair [x]
  (cond
   (empty? x) (throw (Exception. "Argument must not be empty"))
   (empty? (rest x)) x
   :else (recur (rest x))))

;; -----------------------------------------------------------------------------
;; Exercise 2.18
(defn my-reverse [x]
  (letfn [(rev-iter [l result]
            (if (empty? l) result
                (recur (rest l) (cons (first l) result))))]
    (rev-iter x '())))

;; -----------------------------------------------------------------------------
;; Exercise 2.20
(defn same-parity [& items]
  (letfn [(filter-parity [x match-evens matches]
            (if (empty? x) matches
                (recur (rest x)
                       match-evens
                       (if (= (even? (first x)) match-evens) (cons (first x) matches) matches))))]
    (reverse (filter-parity items (even? (first items)) '())))) ;; reverse feels like cheating.

(defn same-parity [& items]
  (let [match-evens (even? (first items))]
    (filter (fn [x] (= (even? x) match-evens)) items)))

;; -----------------------------------------------------------------------------
;; Exercise 2.27
(defn deep-reverse [tree]
  (if (list? tree)
    (reverse (map deep-reverse tree))
    tree))

;; -----------------------------------------------------------------------------
;; Exercise 2.30
(defn square-tree [tree]
  (cond
   (not (list? tree)) (* tree tree)
   (empty? tree) tree
   :else (cons (square-tree (first tree)) (square-tree (rest tree)))))

;; -----------------------------------------------------------------------------
;; Exercise 2.31
(defn tree-map [f tree]
  (if (list? tree)
    (map (fn [t] (tree-map f t)) tree)
    (f tree)))

;; -----------------------------------------------------------------------------
;; Exercise 2.40
(defn unique-pairs [n]
  (letfn [(iter-pairs [i j results]
            (cond 
             (< i 2) results
             (< j 1) (recur (- i 1) (- i 2) results)
             :else (recur i (dec j) (cons (list i j) results))))]
    (iter-pairs n (- n 1) '())))
