(require rackunit)
(require racket/format)
(require racket/base)
(require compatibility/mlist)
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
- Find all signals and their types (done TODO: types)
- Find I/O signals (done TODO: types)
- Find all assignments (done)
- Store signals and assignments in a global environment
- Interpret combinatorial logic
- Interpret register transfer logic

;; Analysis part 2: logic with ASIP
- ASIP instructions definition
- Convert ASIP code to a state machine
|#

;; The environment
;; Consists of signals and assignments
;; There are two environments:
;; 1) consists of all variables and procedures
;; 2) consists of all assignments
;; Once the two have been determined, two things can be done
;; 1) The type of variables can be determined bysed on the usage
;; 2) Everything can be compiled to VHDL after that
;; TODO: optimize: use a better data structure than a list
(define (make-environment (names empty) (vals empty))
  (let ([environment (map cons (reverse names) (reverse vals))])
    (define (is-empty? env) (empty? env))
    (define (is-bound? env name)
      (if (is-empty? env)
          #f
          (or (symbol=? (caar env) name)
              (is-bound? (cdr env) name))))
    (define (ref env name)
      (cond [(is-empty? env)
             (error 'environment-ref
                    "unbound identifier: ~a~n" name)]
            [(symbol=? (caar env) name) (cdar env)]
            [else (ref (cdr env) name)]))
    (define (add! names-vals)
      (set! environment (append names-vals environment)))
    (lambda (command . data)
      (match command
        ['empty? (is-empty? environment)]
        ['is-bound? (is-bound? environment (car data))]
        ['ref (ref environment (car data))]
        ['add! (add! (car data))]
        ['dump environment]))))

(define (environment-is-bound? env name)
  (env 'is-bound? name))
(define (environment-ref env name)
  (env 'ref name))
(define (environment-empty? env)
  (env 'empty?))
(define (environment-add! env names vals)
  (env 'add! (map cons (reverse names) (reverse vals))))
(define (environment-dump env)
  (env 'dump))

;; Test the environment
(define *environment* (make-environment))
(environment-is-bound? *environment* 'e)
(environment-empty? *environment*)
(environment-add! *environment* '(a b c d) '(1 2 3 4))
(environment-dump *environment*)
(environment-is-bound? *environment* 'a)
(environment-ref *environment* 'a)

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

(define (assignment-expression? s-expr)
  (and (list? s-expr)
       (symbol=? 'set (car s-expr))))

(test-case "assignment expression"
           (check-false (assignment-expression?
                         '(def-asip a 10)))
           (check-true (assignment-expression?
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

;; signal/variable/constant structure
(struct signal (name type range initial)
        #:transparent #:mutable)
(struct io signal (io) #:transparent #:mutable)
(struct array (name length type range initial)
        #:transparent #:mutable)
(struct assignment (name range value)
        #:transparent #:mutable)

;; To convert information about signal i/o into structures
;; Superficial analysis in the beginning, upon reading the code
;; In-depth analysis after everything has been read out
(define (analyze-i/o io-exp)
  (define (analyze-i/o-signal expression)
    (define io-tag 'i)
    (match expression
      [(list 'i _)
       (set! io-tag 'i)]
      [(list 'o _)
       (set! io-tag 'o)]
      [(list 'io _)
       (set! io-tag 'io)]
      [whatever
       (error 'analyze-i/o "Unknown i/o type: ~a~n" whatever)])
    (apply io (append (cdr (vector->list
                            (struct->vector
                             (analyze-signal (cadr expression)))))
                      (list io-tag))))
  (map analyze-i/o-signal io-exp))

(analyze-i/o '((o (def oLEDR (range 17 0)))))

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
    [(list 'def name)
     (signal name 'undefined 'undefined 'undefined)]
    [definition (error 'analyze-signal "Error in definition:~n\"~a\"~n" definition)]))


(define (parse-code code)
  (define definitions empty)
  (define assignments empty)
  (define (extend-definitions! new-def)
    (if (list? new-def)
        (set! definitions (append new-def definitions))
        (set! definitions (cons new-def definitions))))
  (define (extend-assignments! new-ass)
    (if (list? new-ass)
        (set! assignments (append new-ass assignments))
        (set! assignments (cons new-ass assignments))))
  (define (sim-eval code)
    (define exp-not-empty? (not (empty? code)))
    (define exp #f)
    (when exp-not-empty?
      (set! exp (car code)))
    (cond [(not exp-not-empty?) #t]
          [(variable? exp)]
          [(vector-definition? exp)
           (sim-eval (cdr code))]
          [(signal-definition? exp)
           (extend-definitions! (analyze-signal exp))
           (sim-eval (cdr code))]
          [(procedure-definition? exp)
           (sim-eval (cdr code))]
          [(i/o-definition? exp)
           (extend-definitions! (analyze-i/o (cdr exp)))
           (sim-eval (cdr code))]
          [(assignment-expression? exp)
           (extend-assignments! (analyze-assignment exp))
           (sim-eval (cdr code))]
          [(conditional? exp)
           (sim-eval (cdr code))]
          [(asip-definition? exp)
           (sim-eval (cdr code))]
          [(loop? exp)
           (sim-eval (cdr code))]
          [else
           (error 'sim-eval "unknown expression: ~a~n" exp)]))
  (sim-eval code)
  (cons definitions assignments))


;; To convert information about signal i/o into structures
;; Superficial analysis in the beginning, upon reading the code
;; In-depth analysis after everything has been read out
(define (analyze-assignment exp)
  (match exp
    [(list 'set name (list from to) value)
     (assignment name (list from to) value)]
    [(list 'set name at value)
     (assignment name at value)]
    [else
     (error 'analyze-assignment "invalid assignment: ~n~a~n" exp)]))


(define (io-type->vhdl io-type)
  (match io-type ['i 'in]['o 'out]['io 'inout]
         [else (error 'io->vhdl "unknown io type ~a~n" io-type)]))

(define (type->vhdl type range)
  (define (analyze-range range)
    (cond [(list? range)
           (sort range >)]
          [(number? range)
           (when (<= range 0)
             (error type->vhdl "bad range: ~a~n range should be either a positive number, or two numbers~n" range))
           (list (- range 1) 0)]
          [(and (symbol? range)
                (symbol=? range 'undefined))
           (error 'type->vhdl "undefined is not good")]))
  (match type
    ['undefined
     (begin
       (define a-range (analyze-range range))
       (~a "std_logic_vector(" (car a-range) " downto " (cadr a-range) ")"))]))

(define (definitions-io defs) (vector-ref defs 0))
(define (definitions-signals defs) (vector-ref defs 1))
(define (definitions-procs defs) (vector-ref defs 2))

(define (definitions-io-set! defs new-val)
  (vector-set! defs 0 new-val))
(define (definitions-signals-set! defs new-val)
  (vector-set! defs 1 new-val))
(define (definitions-procs-set! defs new-val)
  (vector-set! defs 2 new-val))

;; put the IO definitions first, signal definitions second, and procedure definition last
(define (rearrange-definitions definitions)
  (define all-definitions (vector empty empty empty))
  (for ([def definitions])
    (cond [(io? def)
           (definitions-io-set! all-definitions
             (cons def (definitions-io all-definitions)))]
          [(signal? def)
           (definitions-signals-set! all-definitions
             (cons def (definitions-signals all-definitions)))]
          [else
           (definitions-procs-set! all-definitions
             (cons def (definitions-procs all-definitions)))]))
  all-definitions)

;; To convert a list containing definitions and assignments
;; into a string of VHDL code
;; Approach: convert each definition into either a signal (TODO: or procedure)
;; convert each assignment into VHDL
;; TODO: how to convert built-in procedures as opposed to defined procedures?
;; need a list with initial (built-in) procedures
(define (parsed-code->vhdl definitions-assignments)
  (define nl "\n")
  (define all-definitions (rearrange-definitions (car definitions-assignments)))
  (define assignments (cdr definitions-assignments))
  ;; To convert a definition into a VHDL sring
  (define (definition->vhdl definition)
    (match definition
      [(io name type range initial io)
       (~a name " : " (io->vhdl io) " "
           (type->vhdl type range) ";")]
      [else (printf "no match~n")]))
  (define io-list
    (for/list ([io (definitions-io all-definitions)]
               [i (length (definitions-io all-definitions))])
      (define name (signal-name io))
      (define type (signal-type io))
      (define range (signal-range io))
      (define initial (signal-initial io))
      (define io-type (io-io io))
      (~a name " : " (io->vhdl io-type) " " (type->vhdl type range)
          (if (< (+ i 1)
                 (length (definitions-io all-definitions)))
              (string-append ";" nl)
              ""))))
  (define io-string
    (unless (empty? io-list)
      (~a "port (" nl
          (apply string-append io-list) ");")))
  (printf "~a~n" io-string)
  )

(parsed-code->vhdl
 (parse-code
  '(
    (def-i/o ;; maybe def-interface
      ;; inputs and outputs
      (o (def oLEDR (range 7 0)))
      (o (def oLEDG (range 17 0))))
    (set oLEDR 0 1))))

;; Example app
(parse-code
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
