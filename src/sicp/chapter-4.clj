(ns sicp.chapter-4
  (:require [clojure.repl :refer :all])
  (:refer-clojure :rename {eval core-eval}))

(defn eval
  "Evaluates the expression in the context of the given environment."
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

(defn apply
  "Executes the procedure with the given arguments."
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

(defn list-of-values
  "Return the list of values that is the result of evaluating the list of expressions
  in the given environment."
  [exps env]
  (letfn [(iter [rem-exps result]
            (if (no-operands? rem-exps)
              result
              (recur iter (rest rem-exps) (cons (eval (first-operand rem-exps) env) result))))])
  (iter exps '()))

(defn eval-if
  "Evaluates the predicate part of an if statement in the given environment.
  If the result is true, it will evaluate the consequent, otherwise it evaluates
  the alternative."
  [exp env]
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(defn eval-sequence
  "Evaluates a sequence of expressions in the given environment and returns value
  of the final expression."
  [exps env]
  (letfn [(iter [rem-exps]
            (let [result (eval (first-exp rem-exps) env)]
              (if (last-exp? rem-exps)
                result
                (recur (rest-exps rem-exps)))))]))

(defn eval-assignment
  "Evaluates the given expression and binds it to the specified variable in the given environment.
  Returns 'ok on success."
  [exp env]
  (set-variable-value! (assignment-variable exp) (eval (assignment-value exp) env) env)
  'ok)

(defn eval-definition
  "Evaluates the given expression and creates a new variable bound to that value in the given environement.
  Returns 'ok on success."
  [exp env]
  (define-variable! (definition-variable exp) (eval (definition-value exp) env) env)
  'ok)

(defn self-evaluating?
  "Determines whether the expression evaluates to itself."
  [exp]
  (or (number? exp) (string? exp)))

(defn variable?
  "Determines whether the expression is a variable."
  [exp]
  (symbol? exp))

(defn quoted?
  "Determines whether the expression is quoted."
  [exp]
  (tagged-list? exp 'quote))

(defn tagged-list?
  "Returns whether the expression is a list and begins with the specified symbol."
  [exp tag]
  (and (list? exp) (= (first exp) tag)))

(defn assignment?
  "Returns whether the expression is an assignment."
  [exp]
  (tagged-list? exp 'set))

(defn assignment-variable
  "Returns the variable part of an assignment expression."
  [exp]
  (first (rest exp)))

(defn assignment-value
  "Returns the value part of an assignment expression."
  [exp]
  (first (rest (rest exp))))

(defn definition?
  "Returns whether the expression is a definition."
  [exp]
  (tagged-list? exp 'define))

(defn definition-variable
  "Returns the variable part of a definition expression."
  [exp]
  (if (symbol? (first (rest exp)))
    (first (rest exp))
    (first (first (rest exp)))))

(defn definition-value
  "Returns the value part of a definition expression."
  [exp]
  (if (symbol? (first (rest exp)))
    (first (rest (rest exp)))
    (make-lambda (rest (first (rest exp))) (rest (rest exp)))))

(defn lambda?
  "Returns whether the expression is a lambda expression."
  [exp]
  (tagged-list? exp 'lambda))

(defn lambda-parameters
  "Returns the parameters of the given lambda expression."
  [exp]
  (first (rest exp)))

(defn lambda-body
  "Returns the body of the given lambda expression."
  [exp]
  (rest (rest exp)))

(defn make-lambda
  "Constructs a lambda expression from the given parameters and body."
  [parameters body]
  (cons 'lambda (cons parameters body)))

(defn if?
  "Determines whether the expression is an if statement."
  [exp]
  (tagged-list? exp 'if))

(defn if-predicate
  "Returns the predicate part of an if statement."
  [exp]
  (first (rest exp)))

(defn if-consequent
  "Returns the consequent part of an if statement."
  [exp]
  (first (rest (rest exp))))

(defn if-alternative
  "Returns the alternative part of an if statement, if it exists.
  If no alternative was specified, 'false is returned."
  [exp]
  (if (not (empty? (rest (rest (rest exp)))))
    (first (rest (rest (rest exp))))
    'false))

(defn make-if
  "Contructs an if statement from the specified parts."
  [predicate consequent alternative]
  (list 'if predicate consequent alternative))

(defn begin?
  "Determines whether the expression is a begin block."
  (tagged-list? exp 'begin))

(defn begin-actions
  "Returns the actions part of a begin block."
  [exp]
  (rest exp))

(defn last-exp?
  "Returns whether there is only one more expression in the given sequence of expressions."
  [exps]
  (empty? (rest exps)))

(defn first-exp
  "Returns the first expression from a sequence of expressions."
  [exps]
  (first exps))

(defn rest-exps
  "Returns all expressions after the first expression in a sequence of expressions."
  [exps]
  (rest (exps)))
