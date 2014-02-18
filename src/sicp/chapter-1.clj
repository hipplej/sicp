(ns sicp.chapter-1)

;; Linear recursive version of factorial.
(defn factorial [n]
  (if (<= n 1) 1
      (* n (factorial (- n 1)))))

;; Linear iterative version of factorial.
(defn factorial [n]
  (let [factorial-iter (fn [product counter]
                         (if (> counter n)
                           product
                           (recur (* counter product) (+ counter 1))))]
    (factorial-iter 1 1)))

;; Tree recursive version of fibonacci.
(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1))
            (fib (- n 2)))))

;; Linear iterative version of fibonacci.
(defn fib [n]
  (let [fib-iter (fn [curr next counter]
                   (if (= counter 0)
                     curr
                     (recur next (+ next curr) (- counter 1))))]
    (fib-iter 0 1 n)))

;; Exercise 1.11
;; Tree recursive version.
(defn func [n]
  (if (< n 3)
    n
    (+ (func (- n 1)) (* 2 (func (- n 2))) (* 3 (func (- n 3))))))

;; Linear iterative version.
(defn func [n]
  (let [func-iter (fn [a b c counter]
                    (if (= counter 0)
                      a
                      (recur b c (+ c (* 2 b) (* 3 a)) (- counter 1))))]
    (func-iter 0 1 2 n)))

;; Exercise 1.12
(defn pascal-val [row col]
  (cond
   (or (< row col) (< col 0)) 0 ; error conditions
   (or (= col 1) (= col row)) 1 ; base case
   :else (+ (pascal-val (- row 1) (- col 1)) (pascal-val (- row 1) col))))

;; Exercise 1.30
(defn sum [term a next b]
  (let [sum-iter (fn [a result]
                   (if (> a b)
                     result
                     (recur (next a) (+ result (term a)))))]
    (sum-iter a 0)))

;; Exercise 1.31
;; Linear recursive version.
(defn prod [term a next b]
  (if (> a b)
    1
    (* (term a) (prod term (next a) next b))))

;; Linear iterative version.
(defn prod [term a next b]
  (let [prod-iter (fn [a result]
                    (if (> a b)
                      result
                      (recur (next a) (* result (term a)))))]
    (prod-iter a 1)))

(defn prod-factorial [n]
  (prod identity 1 inc n))

(defn est-pi [n]
  (let [pi-term (fn [x]
                  (if (even? x)
                    (/ (+ x 2.0) (+ x 1.0))
                    (/ (+ x 1.0) (+ x 2.0))))]
    (* 4 (prod pi-term 1 inc n))))

;; Exercise 1.34
(defn f [g]
  (g 2))

;; sicp.chapter-1> (f f)
;; ClassCastException java.lang.Long cannot be cast to clojure.lang.IFn  sicp.chapter-1/f (form-init8955144122712306284.clj:2)

;; Exercise 1.37
;; Linear Recursive version.
(defn cont-frac [n d k]
  (letfn [(cont-frac-recur [i]
            (if (> i k) 0
                (/ (n i) (+ (d i) (cont-frac-recur (inc i))))))]
    (cont-frac-recur 1)))

;; Linear Iterative version.
(defn cont-frac [n d k]
  (let [cont-frac-iter (fn [i result]
                         (if (= i 0)
                           result
                           (recur (dec i) (/ (n i) (+ (d i) result)))))]
    (cont-frac-iter k 0.0)))
