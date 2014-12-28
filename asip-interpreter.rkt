(require racket/base)
(require rackunit)
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
- Find all signals and their types
- Find I/O signals
- Interpret combinatorial logic
- Interpret register transfer logic

;; Analysis part 2: logic with ASIP
- ASIP instructions definition
- Convert ASIP code to a state machine
|#


(define (variable? s-expr)
  (symbol? s-expr))

;; Test cases
(test-case "variable" (check-true (variable? 'a)))

;; to check whether an s-expression is a signal definition
;; differentiate between 
(define (signal-definition? s-expr)
  (and (list? s-expr)
       (symbol=? 'def (car s-expr))
       (not (empty? (cdr s-expr)))
       (symbol? (cadr s-expr))))

(define (vector-definition? s-expr)
  (and (list? s-expr)
       (symbol=? 'def-vector (car s-expr))))

;; Test cases
(test-case "signal definition"
           (check-true (signal-definition?
                        '(def a 10))))
(test-case "procedure definition"
           (check-false (signal-definition?
                         '(def (a) 10))))
(test-case "vector of signals definition"
           (check-true (vector-definition?
                        '(def-vector (a) 10))))


(define (procedure-definition? s-expr)
  (and (list? s-expr)
       (symbol=? 'def (car s-expr))
       (not (empty? (cdr s-expr)))
       (list? (cadr s-expr))))

;; Test cases
(test-case "signal definition"
           (check-false (procedure-definition?
                         '(def a 10))))
(test-case "procedure definition"
           (check-true (procedure-definition?
                        '(def (a) 10))))
(test-case "vector of signals definition"
           (check-false (procedure-definition?
                         '(def-vector (a) 10))))

(define (i/o-definition? s-expr)
  (and (list? s-expr)
       (symbol=? 'def-i/o (car s-expr))))

(test-case "i/o definition"
           (check-true (i/o-definition?
                        '(def-i/o a 10))))


(define (asip-definition? s-expr)
  (and (list? s-expr)
       (symbol=? 'def-asip (car s-expr))))

(test-case "asip definition"
           (check-true (asip-definition?
                        '(def-asip a 10))))

(define (assignment? s-expr)
  (and (list? s-expr)
       (symbol=? 'set (car s-expr))))

(test-case "assignment"
           (check-false (assignment?
                         '(def-asip a 10)))
           (check-true (assignment?
                        '(set a 10))))

(define (conditional? s-expr)
  (and (list? s-expr)
       (or (symbol=? 'when (car s-expr))
           (symbol=? 'if (car s-expr))
           (symbol=? 'cond (car s-expr)))))

(test-case "conditional"
           (check-true (conditional?
                        '(when a 10)))
           (check-true (conditional?
                        '(if a 10))))

(define (loop? s-expr)
  (and (list? s-expr)
       (symbol=? 'for (car s-expr))))

(test-case "loop"
           (check-true (loop?
                        '(for a 10))))


;; To convert information about signal i/o into structures
;; Superficial analysis in the beginning, upon reading the code
;; In-depth analysis after everything has been read out
(define (analyze-i/o an-i/o)
  (define (analyze-i/o-signal expression)
    (match expression
      [(list definitions ...)]))
  

  )

;; signal/variable/constant structure
(struct signal (name type range initial)
        #:transparent #:mutable)
(struct array (name length type range initial)
        #:transparent #:mutable)

;; To convert information about signal/variable/constant
;; into a structure
;; Superficial analysis in the beginning, upon reading the code
;; In-depth analysis after everything has been read out
;; TODO: figure out the type based on signal usage context
(define (analyze-signal a-signal)
  (match a-signal
    [(list 'def-vector name length (list 'range from to) init)
     (array name length 'undefined (list from to) init)]
    [(list 'def-vector name length (list 'range from to))
     (array name length 'undefined (list from to)
            'undefined)]
    [(list 'def name (list 'range from to) init)
     (signal name 'undefined (list from to) init)]
    [(list 'def name (list 'range from to))
     (signal name 'undefined (list from to) 'undefined)]
    [(list 'def name (list init-vals ...))
     (define a-range init-vals)
     (define from 0)
     (define to (- (length a-range) 1))
     (signal name 'undefined (list from to) init-vals)]
    [(list 'def name init)
     (signal name 'undefined 'undefined init)]
    [definition (error 'analyze-signal "Error in definition:~n\"~a\"~n" definition)]))


(define (sim-eval code)
  (define exp-not-empty? (not (empty? code)))
  (define exp #f)
  (when exp-not-empty?
    (set! exp (car code)))
  (cond [(not exp-not-empty?) #t]
        [(variable? exp)]
        [(vector-definition? exp)
         (printf "deriving vector~n")
         (sim-eval (cdr code))]
        [(signal-definition? exp)
         (printf "deriving signal: ~a~n"
                 (analyze-signal exp))
         (sim-eval (cdr code))]
        [(procedure-definition? exp)
         (printf "procedure definition~n")
         (sim-eval (cdr code))]
        [(i/o-definition? exp)
         (printf "i/o definition~n")
         (sim-eval (cdr code))]
        [(assignment? exp)
         (printf "assignment~n")
         (sim-eval (cdr code))]
        [(conditional? exp)
         (printf "conditional~n")
         (sim-eval (cdr code))]
        [(asip-definition? exp)
         (printf "asip definition~n")
         (sim-eval (cdr code))]
        [(loop? exp)
         (printf "loop~n")
         (sim-eval (cdr code))]
        [else
         (error 'sim-eval "unknown expression: ~a~n" exp)]))


;; Example app
(sim-eval
 '(
   (def-i/o ;; maybe def-interface
     ;; inputs and outputs
     (i (def iCLK_50))
     (i (def iKEY (range 3 0)))
     (o (def oLEDR (range 17 0)))
     (o (def oLEDG (range 7 0)))
     (io (def GPIO_0 (range 31 0))))
   
   ;; registers, wires
   (def a (range 10 0) 10)
   (def ab (range 10 0) 10)
   (def b (range 10 0) 10)
   (def c (range 10 0) (others 0)) ;; set range and default values
   (def d (make-list 10 0)) ;; set default values only, derive range
   (def e (list 1 0 0 1 1 1 0))
   (def N 10) ;; constant
   
   ;; looped signal definition
   ;; will be expanded into 10 signals/registers
   (def-vector c N (range 10 0))

   ;; looped processing
   (for ([i (range 0 10)])
     (set a 0 (ref (* i 10) 0)))

   ;; register logic
   (when (rising-edge iCLK_50)
     (def temp (ref iKEY 0))
     (set c 0 (xor c temp)))

   ;; combinatorial logic
   (set b (range 3 0) (ref iKEY (range 3 0)))
   (set b (at 10 9 8 7) (ref iKEY (at 3 2 1 0)))

   ;; ASIP instructions
   (def (add-regs)
        (set c (+ c 10))
        (set b (+ c 20)))

   (def-asip a00
     (for ([i 0 10]) ;; come out as a counter
       (add-regs)
       (add-regs)))))
