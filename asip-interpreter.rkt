;; Current problem with the macro solution:
;; two definitions of the same thing:
;; 1) simulator,
;; 2) conversion to vhdl code

;; Possible solution:
;; an interpreter to convert the DSL code into some form that can
;; be both: simulated and converted to VHDL based on user choice

;; Another way:
;; Two interpreters (or one interpreter and one compiler)
;; for the same code
;; 1) The first one simulated the code
;; 2) The second converts it to VHDL

;; The simulator should support concurrent execution
;; Should probably add delays for all standard operations

(define (sim-eval code)
  ;; traverse the code and find/merge:
  ;; ASIP description---ASIP instructions
  ;; ASIP code
  ;; signal/register definitions
  ;; external I/O that can be set by the user
  ;; returns a sorted list of all definitions
  (define (traverse-and-merge code)
    )
  ;; extract necessary state from the code
  ;; registers, initialize them
  )


;; Simulator: Interpreter approach
#|
- Analyze the expressions 
- Execute the expressions

- It is important to analyze the expressions before their execution because many will be executed "in parallel".
- A merger of SICP evaluator and SICP circuit simulator is needed.
- The circuit simulator can be used to simulate the parallel events of a circuit that are allowed in ASIP.

;; Analysis
- Which wires and registers are there?
- What will be executed next (scheduler)?
- What is the result of execution?
- Sequential ASIP instruction execution

|#

;; VHDL-Compiler: Interpreter approach
#|
- Analyze the expressions 
- Compile to VHDL

;; Analysis part 1: logic without ASIP
- Find all signnals and their types
- Find I/O signals
- 

;; Analysis part 2: logic without ASIP
- ASIP instructions definition
- Convert ASIP code to a state machine

|#




(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type - EVAL" exp))))



(eval '(define a 10) empty)




(define (interpreter code)
  ())

(define empty-env
  (lambda () (list 'empty-env)))

(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))


(define apply-env
  (lambda (env search-var)
    (cond
     ((eqv? (car env) 'empty-env)
      (report-no-binding-found search-var))
     ((eqv? (car env) 'extend-env)
      (let ((saved-var (cadr env))
            (saved-val (caddr env))
            (saved-env (cadddr env)))
        (if (eqv? search-var saved-var)
            saved-val
            (apply-env saved-env search-var))))
     (else
      (report-invalid-env env)))))


(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env "No binding for ~s" search-var)))
(define report-invalid-env
  (lambda (env)
    (error 'apply-env "Bad environment: ~s" env)))


(define (var-exp var) var)
(define (lambda-exp vars lc-exp) `(lambda ,vars ,lc-exp))
(define (app-exp var lc-exp) `(lambda (,var) ,lc-exp))

(define occurs-free?
  (lambda (search-var exp)
    (cond
     ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
     ((lambda-exp? exp)
      (and
       (not (eqv? search-var (lambda-exp->bound-var exp)))
       (occurs-free? search-var (lambda-exp->body exp))))
     (else
      (or
       (occurs-free? search-var (app-exp->rator exp))
       (occurs-free? search-var (app-exp->rand exp)))))))


(define (parse-expression datum)
  (cond
   ((symbol? datum) (var-exp datum))
   ((pair? datum)
    (if (eqv? (car datum) 'lambda)
        (lambda-exp
         (car (cadr datum))
         (parse-expression (caddr datum)))
        (app-exp
         (parse-expression (car datum))
         (parse-expression (cadr datum)))))
   (else (report-invalid-concrete-syntax datum))))

(parse-expression '(define a 10))


;; Racket style code looks
(hw
 ;; inputs and outputs
 (def-i/o (list (iCLK_50 in)
                (iKEY in 3 0)
                (oLEDR out 17 0)
                (oLEDG out 7 0)))

 ;; registers, wires
 (def a (range 10 0) 10)
 (def b (range 10 0) 10)
 (def N 10) ;; constant
 
 ;; looped signal definition
 ;; will be expanded into 10 signals/registers
 (def-vector c N (range 10 0))

 ;; register logic
 (when (rising-edge iCLK_50)
   (def temp (ref iKEY 0))
   (set c 0 (xor c temp)))

 ;; combinatorial logic
 (set b (range 3 0) (ref iKEY (range 3 0)))
 (set b (at 10 9 8 7) (ref iKEY (at 3 2 1 0)))

 ;; ASIP instructions
 (def-instruction (shift-regs)
   (set (ref c (range 9 1)) (ref c (range 8 0))))

 (def (add-regs)
      (set c (+ c 10))
      (set b (+ c 20)))

 (define-asip a00
   (for ([i 0 10]) ;; come out as a counter
     (add-regs)
     (add-regs))
 )

;; here is how user code will look like (python style):
;; def-i/o:
;;   iCLK_50 in
;;   iKEY    in  3  0
;;   oLEDR   out 17 0
;;   oLEDG   out 7  0
