(ns sicp.chapter-4
  (:require [clojure.repl :refer :all])
  (:refer-clojure :rename {eval core-eval
                           apply core-apply}))

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
   (do? exp) (eval-sequence (do-actions exp) env)
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

(defn do?
  "Determines whether the expression is a do block."
  (tagged-list? exp 'do))

(defn do-actions
  "Returns the actions part of a do block."
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

(defn sequence->exp
  "Transforms a sequence of expressions into a single expression."
  [exps]
  (cond
   (empty? exps) exps
   (last-exp? exps) (first-exp exps)
   :else (make-do exps)))

(defn make-do
  "Constructs a do block that encloses a sequence of expressions."
  [exps]
  (cons 'do exps))

(defn application?
  "Determines whether the expression is a procedure application."
  [exp]
  (list? exp))

(defn operator
  "Returns the operator of the procedure application."
  [exp]
  (first exp))

(defn operands
  "Returns the operands of the procedure application."
  [exp]
  (rest exp))

(defn no-operands?
  "Returns whether there are no operands remaining in the list of operands."
  [ops]
  (empty? ops))

(defn first-operand
  "Returns the first operand in the list of operands."
  [ops]
  (first ops))

(defn rest-operands
  "Returns all operands after the first operand in the list of operands."
  [ops]
  (rest ops))

(defn cond?
  "Returns whether the expression is a cond block."
  [exp]
  (tagged-list? exp 'cond))

(defn cond-clauses
  "Returns the clauses of the cond block."
  [exp]
  (rest exp))

(defn cond-else-clause?
  "Returns whether the specified cond clause is an else clause."
  [clause]
  (= (cond-predicate clause) :else))

(defn cond-predicate
  "Returns the predicate of the specified cond clause."
  [clause]
  (first clause))

(defn cond-actions
  "Returns the actions of the specified cond clause."
  [clause]
  (rest clause))

(defn cond->if
  "Converts a cond block into a series of nested if statements."
  [exp]
  (letfn [(iter [rem-clauses]
            (let [first-clause (first rem-clauses)
                  rest-clauses (rest rem-clauses)]
              (cond
               (cond-else-clause? first-clause) (if (empty? rest-clauses)
                                                  (sequence->exp (cond-actions first))
                                                  (throw (Exception. ":else clause isn't in final slot.")))
               (empty? rest-clauses) 'false
               :else (make-if (cond-predicate first-clause)
                         (sequence->exp (cond-actions first-clause))
                         (recur rest-clauses)))))]
    (iter exp)))

(defn make-procedure
  "Constructs a procedure."
  [parameters body env]
  (list 'procedure parameters body env))

(defn compount-procedure?
  "Determines whether the procedure is a compount procedure."
  [proc]
  (tagged-list? proc 'procedure))

(defn procedure-parameters
  "Returns the parameters of the given procedure."
  [proc]
  (first (rest proc)))

(defn procedure-body
  "Returns the body of the given procedure."
  [proc]
  (first (rest (rest proc))))

(defn procedure-environment
  "Returns the environment of the given procedure."
  [proc]
  (first (rest (rest (rest proc)))))
