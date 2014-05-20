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
(defn last-pair [items]
  (cond
   (empty? items) (throw (Exception. "Argument must not be empty"))
   (empty? (rest items)) items
   :else (recur (rest items))))

;; -----------------------------------------------------------------------------
;; Exercise 2.18
;; Linear iterative.
(defn my-reverse [items]
  (letfn [(rev-iter [sub-items result]
            (if (empty? sub-items) result
                (recur (rest sub-items) (cons (first sub-items) result))))]
    (rev-iter items '())))

;; -----------------------------------------------------------------------------
;; Exercise 2.20
;; Linear recursive.
(defn same-parity [& items]
  (letfn [(matches-parity [item]
            (= (even? (first items)) (even? item)))
          (filter-parity [sub-items]
            (cond
             (empty? sub-items) sub-items
             (matches-parity (first sub-items)) (cons (first sub-items) (filter-parity (rest sub-items)))
             :else (filter-parity (rest sub-items))))]
    (filter-parity items)))

;; Linear iterative.
(defn same-parity [& items]
  (letfn [(matches-parity [item]
            (= (even? (first items)) (even? item)))
          (filter-parity [sub-items matches]
            (if (empty? sub-items) matches
                (recur (rest sub-items)
                       (if (matches-parity (first sub-items))
                         (cons (first sub-items) matches)
                         matches))))]
    (reverse (filter-parity items '())))) ;; reverse feels like cheating.

;; Better linear iterative.
(defn same-parity [& items]
  (letfn [(matches-parity [item]
            (= (even? (first items)) (even? item)))]
    (filter matches-parity items)))

;; -----------------------------------------------------------------------------
;; Exercise 2.27
(defn deep-reverse [items]
  (if (not (list? items))
    items
    (reverse (map deep-reverse items))))

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
  (if (not (list? tree))
    (f tree)
    (map (fn [t] (tree-map f t)) tree)))

;; -----------------------------------------------------------------------------
;; Exercise 2.40
(defn unique-pairs [n]
  (letfn [(iter-pairs [i j results]
            (cond
             (< i 2) results
             (< j 1) (recur (- i 1) (- i 2) results)
             :else (recur i (- j 1) (cons (list i j) results))))]
    (iter-pairs n (- n 1) '())))
