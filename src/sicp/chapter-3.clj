(ns sicp.chapter-3)

;; -----------------------------------------------------------------------------
;; Exercise 3.1
(defn make-accumulator [init]
  (let [sum (atom init)]
    (fn [x]
      (swap! sum + x)
      @sum)))

;; -----------------------------------------------------------------------------
;; Exercise 3.2
(defn make-monitored [f]
  (let [counter (atom 0)]
    (fn [x]
      (cond
       (= x 'how-many-calls) @counter
       (= x 'reset-count) (reset! counter 0)
       :else (do (swap! counter inc) (f x))))))

;; -----------------------------------------------------------------------------
;; Exercises 3.3, 3.4, and 3.7
(defn make-account [init secret]
  (let [balance (atom init)
        bad-guess-count (atom 0)
        passwords (atom (set '(secret)))]
    (fn [guess action]
      (letfn [(withdraw [amount]
                (if (< @balance amount)
                  (println "Insufficient funds")
                  (swap! balance - amount)))
              (deposit [amount]
                (swap! balance + amount))
              (add-password [new-password]
                (swap! passwords conj new-password))
              (note-bad-guess [amount]
                (swap! bad-guess-count inc)
                (println "Incorrect password"))
              (call-the-cops [amount]
                (println "Calling the cops"))]
        (cond
         (> @bad-guess-count 6) call-the-cops
         (not (contains? @passwords guess)) note-bad-guess
         (= action 'add-password) add-password
         (= action 'withdraw) withdraw
         (= action 'deposit) deposit
         :else (throw (Exception. "Unknown request")))))))

(defn make-joint [acc acc-password new-password]
  ((acc acc-password 'add-password) new-password)
  acc)
