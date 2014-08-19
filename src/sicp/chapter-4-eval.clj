(ns sicp.chapter-4-eval
  (:require [clojure.repl :refer :all])
  ;; Rename Clojure's built-in eval and apply out of the way.
  (:refer-clojure :rename {eval core-eval
                           apply core-apply}))

;; This is the list of primitive procedures that are supported in our language.
(def primitive-procedures
  (list
   (list 'first first)
   (list 'rest rest)
   (list 'cons cons)
   (list 'empty? empty?)
   (list '+ +)
   (list '- -)
   (list '* *)
   (list '/ /)
   (list '= =)
   (list '> >)
   (list '< <)
   (list 'not not)
   ;; 'and' and 'or' are macros so we have to make functions for them.
   (list 'and (fn [& xs] (reduce #(and %1 %2) true xs)))
   (list 'or (fn [& xs] (reduce #(or %1 %2) false xs)))))

;; =============================================================================
;; Selectors
;; =============================================================================

(defn tagged-list?
  "Returns whether the expression is a list and begins with the symbol."
  [exp tag]
  (and (seq? exp) (= (first exp) tag)))

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

(defn text-of-quotation
  "Returns the quoted text."
  [exp]
  (first (rest exp)))

(defn assignment?
  "Returns whether the expression is an assignment."
  [exp]
  (tagged-list? exp 'set!))

(defn assignment-variable
  "Returns the variable part of an assignment expression."
  [exp]
  (first (rest exp)))

(defn assignment-value
  "Returns the value part of an assignment expression."
  [exp]
  (first (rest (rest exp))))

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
  "Contructs an if statement from the parts."
  [predicate consequent alternative]
  (list 'if predicate consequent alternative))

(defn lambda?
  "Returns whether the expression is a lambda expression."
  [exp]
  (tagged-list? exp 'lambda))

(defn lambda-parameters
  "Returns the parameters of the lambda expression."
  [exp]
  (first (rest exp)))

(defn lambda-body
  "Returns the body of the lambda expression."
  [exp]
  (rest (rest exp)))

(defn make-lambda
  "Constructs a lambda expression from the parameters and body."
  [parameters body]
  (cons 'lambda (cons parameters body)))

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

(defn begin?
  "Determines whether the expression is a begin block."
  [exp]
  (tagged-list? exp 'begin))

(defn begin-actions
  "Returns the actions part of a begin block."
  [exp]
  (rest exp))

(defn last-exp?
  "Returns whether there is only one more expression in the sequence of expressions."
  [exps]
  (empty? (rest exps)))

(defn first-exp
  "Returns the first expression from a sequence of expressions."
  [exps]
  (first exps))

(defn rest-exps
  "Returns all expressions after the first expression in a sequence of expressions."
  [exps]
  (rest exps))

(defn make-begin
  "Constructs a begin block that encloses a sequence of expressions."
  [exps]
  (cons 'begin exps))

(defn sequence->exp
  "Transforms a sequence of expressions into a single expression."
  [exps]
  (cond
   (empty? exps) exps
   (last-exp? exps) (first-exp exps)
   :else (make-begin exps)))

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

(defn cond-predicate
  "Returns the predicate of the cond clause."
  [clause]
  (first clause))

(defn cond-else-clause?
  "Returns whether the cond clause is an else clause."
  [clause]
  (= (cond-predicate clause) 'else))

(defn cond-actions
  "Returns the actions of the cond clause."
  [clause]
  (rest clause))

(defn expand-clauses
  "Expands cond block clauses into nested if statements."
  [clauses]
  (if (empty? clauses)
    'false
    (let [first-clause (first clauses)
          rest-clauses (rest clauses)]
      (if (cond-else-clause? first-clause)
        (if (empty? rest-clauses)
          (sequence->exp (cond-actions first-clause))
          (throw (Exception. "else clause isn't in final slot")))
        (make-if (cond-predicate first-clause)
                 (sequence->exp (cond-actions first-clause))
                 (expand-clauses rest-clauses))))))

(defn cond->if
  "Converts a cond block into a series of nested if statements."
  [exp]
  (expand-clauses (cond-clauses exp)))

(defn make-procedure
  "Constructs a procedure."
  [parameters body env]
  (list 'procedure parameters body env))

(defn compound-procedure?
  "Determines whether the procedure is a compound procedure."
  [proc]
  (tagged-list? proc 'procedure))

(defn procedure-parameters
  "Returns the parameters of the procedure."
  [proc]
  (first (rest proc)))

(defn procedure-body
  "Returns the body of the procedure."
  [proc]
  (first (rest (rest proc))))

(defn procedure-environment
  "Returns the environment of the procedure."
  [proc]
  (first (rest (rest (rest proc)))))

(defn primitive-procedure?
  "Returns whether the procedure is a primitive procedure."
  [proc]
  (tagged-list? proc 'primitive))

(defn primitive-implementation
  "Returns the implementation part of a primitive procedure."
  [proc]
  (first (rest proc)))

(defn primitive-procedure-names
  "Returns the names of the primitive procedures."
  []
  (map first primitive-procedures))

(defn primitive-procedure-objects
  "Returns the objects of the primitive procedures."
  []
  (map (fn [proc] (list 'primitive (first (rest proc)))) primitive-procedures))

(defn apply-primitive-procedure
  "Calls the Clojure function that is associated with the primitive procedure."
  [proc args]
  (core-apply (primitive-implementation proc) args))

;; =============================================================================
;; Environment Functions
;; =============================================================================

;; Environments are list of frames (maps).
(def the-empty-environment '())

(defn enclosing-environment
  "Returns the environment that encloses the environment."
  [env]
  (rest env))

(defn first-frame
  "Returns the first frame of the environment."
  [env]
  (first env))

(defn make-frame
  "Constructs an environment frame where the variable
  are bound to the values."
  [variables values]
  (atom (zipmap variables values)))

(defn add-binding-to-frame
  "Binds the value to the variable in the frame."
  [variable value frame]
  (swap! frame assoc variable value))

(defn extend-environment
  "Extends the environment with a new stack frame with bindings for the
  variables and values."
  [variables values base-env]
  (if (= (count variables) (count values))
    (cons (make-frame variables values) base-env)
    (if (< (count variables) (count values))
      (throw (Exception. (format "Too many arguments supplied - variables: %s, values: %s" variables values)))
      (throw (Exception. (format "Too few arguments supplied - variables: %s, values: %s" variables values))))))

(defn lookup-variable-value
  "Returns the value of first binding for variable in environment."
  [variable env]
  (letfn [(env-loop [rem-env]
            (cond
             (= rem-env the-empty-environment) (throw (Exception. (format "Unbound variable: %s" variable)))
             (contains? (deref (first-frame rem-env)) variable) (get (deref (first-frame rem-env)) variable)
             :else (recur (enclosing-environment rem-env))))]
    (env-loop env)))

(defn set-variable-value!
  "Binds an existing variable to the value in the environment."
  [variable value env]
  (letfn [(env-loop [rem-env]
            (cond
             (= rem-env the-empty-environment) (throw (Exception. (format "Unbound variable: %s" variable)))
             (contains? (deref (first-frame rem-env)) variable) (add-binding-to-frame variable value  (first-frame rem-env))
             :else (recur (enclosing-environment rem-env))))]
    (env-loop env)))

(defn define-variable!
  "Binds a variable to the value in the environment, creating a new variable if necessary."
  [variable value env]
  (letfn [(env-loop [rem-env]
            (cond
             (= rem-env the-empty-environment) (add-binding-to-frame variable value (first-frame env))
             (contains? (deref (first-frame rem-env)) variable) (add-binding-to-frame variable value (first-frame rem-env))
             :else (recur (enclosing-environment rem-env))))]
    (env-loop env)))

(defn setup-environment
  "Sets up a relatively sparse environment."
  []
  (let [initial-env (extend-environment (primitive-procedure-names)
                                        (primitive-procedure-objects)
                                        the-empty-environment)]
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;; =============================================================================
;; Helper functions for eval
;; =============================================================================

;; Forward declare eval since these all depend on it.
(declare eval)

(defn list-of-values
  "Returns the list of values that is the result of evaluating the list of expressions
  in the environment."
  [exps env]
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env) (list-of-values (rest-operands exps) env))))

(defn eval-if
  "Evaluates the predicate part of an if statement in the environment.
  If the result is true, it will evaluate the consequent, otherwise it evaluates
  the alternative."
  [exp env]
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(defn eval-sequence
  "Evaluates a sequence of expressions in the environment and returns value
  of the final expression."
  [exps env]
  (let [result (eval (first-exp exps) env)]
    (if (last-exp? exps)
      result
      (eval-sequence (rest-exps exps) env))))

(defn eval-assignment
  "Evaluates the expression and binds it to the variable in the environment.
  Returns 'ok on success."
  [exp env]
  (set-variable-value! (assignment-variable exp) (eval (assignment-value exp) env) env)
  'ok)

(defn eval-definition
  "Evaluates the expression and creates a new variable bound to that value in the environement.
  Returns 'ok on success."
  [exp env]
  (define-variable! (definition-variable exp) (eval (definition-value exp) env) env)
  'ok)

;; =============================================================================
;; Metacircular Evaluator
;; =============================================================================
(defn apply
  "Executes the procedure with the arguments."
  [procedure arguments]
  (cond
   (primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)
   (compound-procedure? procedure) (eval-sequence
                                    (procedure-body procedure)
                                    (extend-environment
                                     (procedure-parameters procedure)
                                     arguments
                                     (procedure-environment procedure)))
   :else (throw (Exception. (format "Unknown procedure type: %s" procedure)))))

(defn eval
  "Evaluates the expression in the context of the environment."
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

;; =============================================================================
;; REPL stuff
;; =============================================================================
(def the-global-environment (setup-environment))

(defn format-object
  "Special formatter for printing out results without printing the environment part."
  [object]
  (if (compound-procedure? object)
    (list 'compound-procedure
          (procedure-parameters object)
          (procedure-body object)
          '<procedure-env>)
    object))

(defn repl
  "Implements a simple REPL for our evaluator."
  []
  (letfn [(read-val []
            (print ";;; M-Eval input: ")
            (let [input (read)]
              (print input)
              input))
          (eval-val [input]
            (eval input the-global-environment))
          (print-val [output]
            (println (format "\n;;; M-EVAL value: %s\n" (format-object output))))]
    (loop []
      (let [input (read-val)]
        (if (= input 'quit)
          'bye
          (do
            (print-val (eval-val input))
            (recur)))))))
