(ns sicp.chapter-4
  (:require [clojure.repl :refer :all])
  (:refer-clojure :rename {eval core-eval}))

(defn list-of-values
  "Return the list of values that is the result of evaluating the list of expressions
   in the environment."
  [exps env]
  (letfn [(iter [rem-exps result]
            (if (no-operands? rem-exps)
              result
              (recur iter (rest rem-exps) (cons (eval (first-operand rem-exps) env) result))))])
  (iter exps '()))

(defn apply
  "Executes the procedure with the provided arguments."
  [procedure arguments]
  (cond
   (primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)
   (compount-procedure? procedure) (eval-sequence
                                    (procedure-body procedure)
                                    (extend-environment
                                     (procedure-parameters procedure)
                                     arguments
                                     (procedure-environment procedure)))
   :else (throw (Exception. (format "Unknown procedure type: %s" procedure)))))

(defn eval
  "Evaluates the expression in the context of the provided environment."
  [exp env]
  (cond
   (self-evaluating? exp) exp
   (variable? exp) (lookup-variable-value exp env)
   (quoted? exp) (text-of-quotation exp)
   (assignment? exp) (eval-assignment exp env)
   (definition? exp) (eval-definition exp env)
   (if? exp) (eval-if exp env)
   (lambda? exp) (make-procedure
                  (lambda-parameters exp)
                  (lambda-body exp)
                  env)
   (begin? exp) (eval-sequence (begin-actions exp) env)
   (cond? exp) (eval (cond->if exp) env)
   (application? exp) (apply (eval (operator exp) env)
                             (list-of-values (operands exp) env))
   :else (throw (Exception. (format  "Unknown expression type: %s" exp)))))
