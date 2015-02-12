(module asip-interpreter racket
  (provide (all-defined-out))
  (require rackunit
           racket/format
           "asip-vhdl.rkt")

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
    (cons definitions (reverse assignments)))


  ;; To convert information about signal i/o into structures
  ;; Superficial analysis in the beginning, upon reading the code
  ;; In-depth analysis after everything has been read out
  (define (analyze-assignment exp)
    (match exp
      [(list 'set name value)
       (assignment name #f value)]
      [(list 'set name (list from to) value)
       (assignment name (list from to) value)]
      [(list 'set name at value)
       (assignment name at value)]
      [else
       (error 'analyze-assignment "invalid assignment: ~n~a~n" exp)]))


  (define (io-type->vhdl io-type)
    (match io-type
      ['i 'in]
      ['o 'out]
      ['io 'inout]
      [else (error 'io-type->vhdl "unknown io type ~a~n" io-type)]))

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


  ;; VHDL function struct
  ;; the type: 'prefix 'infix
  ;; arity
  (struct function (name type arity) #:transparent #:mutable)
  (struct procedure (name arity) #:transparent #:mutable)
  (struct special-form (name code) #:transparent #:mutable)

  (define (make-built-in-functions)
    (list
     (function '+ 'infix 2)
     (function '- 'infix 2)
     (function 'not 'prefix 1)
     (function 'and 'infix 2)
     (function 'or 'infix 2)
     (function '* 'infix 2 )))

  (define (function->vhdl fn args)
    (define arity (function-arity fn))
    (when (not (= arity (length args)))
      (error (function-name fn) "function arity should be ~a, but is ~a instead~n" arity (length args)))
    (if (eq? (function-type fn) 'infix)
        (~a "(" (car args) " " (function-name fn) " " (cadr args)")")
        (~a "("(function-name fn) " " (car args) ")")))

  (define (make-built-in-procedures)
    (list
     (procedure 'rising-edge? 1)
     (procedure 'to_integer 2)
     (procedure 'std_logic_vector 1)
     (procedure 'unsigned 1)))

  (define (procedure->vhdl proc args)
    (define arity (procedure-arity proc))
    (when (not (= arity (length args)))
      (error (procedure-name proc) "procedure arity should be ~a, but is ~a instead~n" arity (length args))))


  (define (make-built-in-special-forms)
    (list
     (procedure 'when 'when)
     (procedure 'for 'for)))

  (define (special-form->vhdl form args)
    (special-form-name form))

  (define (find-definition name accessor definitions)
    (define (find-local definitions)
      (cond [(empty? definitions) #f]
            [(eq? name (accessor (car definitions))) (car definitions)]
            [else (find-local (cdr definitions))]))
    (find-local definitions))

  ;; To convert a list containing definitions and assignments
  ;; into a string of VHDL code
  ;; Approach: convert each definition into either a signal (TODO: or procedure)
  ;; convert each assignment into VHDL
  ;; TODO: how to convert built-in procedures as opposed to defined procedures?
  ;; need a list with initial (built-in) procedures
  (define (parsed-code->vhdl entity-name definitions-assignments)
    (define nl "\n")
    (define all-definitions (rearrange-definitions
                             (car definitions-assignments)))
    (define assignments (cdr definitions-assignments))
    (define functions (make-built-in-functions))
    (define procedures (make-built-in-procedures))
    (define special-forms (make-built-in-special-forms))
    ;; To convert a definition into a VHDL sring
    (define (io-definition->vhdl definition)
      (match definition
        [(io name type range initial io)
         (~a name " : " (io-type->vhdl io) " "
             (type->vhdl type range))]
        [else (printf "no match~n")]))
    (define io-list
      (for/list ([io (definitions-io all-definitions)]
                 [i (length (definitions-io all-definitions))])
        (~a (io-definition->vhdl io)
            (if (< (+ i 1) (length (definitions-io all-definitions)))
                (string-append ";" nl)
                ""))))
    (define io-string
      (if (empty? io-list)
          ""
          (~a "port (" nl
              (apply string-append io-list) ");")))
    (define generics-string "")
    (define signals-string "")
    (define (expression->vhdl exp)
      ;; convert all expressions on the list
      (cond [(list? exp)
             (define name (car exp))
             ;; search the name in functions, procedures and special forms
             (define definition (find-definition name
                                                 function-name
                                                 functions))
             (if definition
                 (function->vhdl definition
                                 (map expression->vhdl (cdr exp)))
                 (let [(definition (find-definition name
                                                    procedure-name
                                                    procedures))]
                   (if definition
                       (procedure->vhdl definition
                                        (map expression->vhdl
                                             (cdr exp)))
                       (let [(definition (find-definition
                                          name
                                          special-form-name
                                          special-forms))]
                         (if definition
                             (special-form->vhdl
                              definition
                              (map expression->vhdl
                                   (cdr exp)))
                             (error "expression not found~a~n"
                                    name))))))]
            [else exp]))
    ;; only simple assignments for now
    (define (assignment->vhdl assignment)
      (~a (assignment-name assignment)
          (if (assignment-range assignment)
              (let ([range (assignment-range assignment)])
                (~a "("
                    (if (list? range)
                        (if (> (car range) (cadr range))
                            (~a (car range) " downto " (cadr range))
                            (~a (car range) " to " (cadr range)))
                        range)
                    ")"))
              "")
          " <= " (expression->vhdl (assignment-value assignment))
          ";\n"))
    (define assignments-string
      (apply string-append
             (map assignment->vhdl assignments)))
    (~a (vhdl-standard-libraries)
        (vhdl-entity entity-name io-string generics-string)
        (vhdl-architecture entity-name
                           signals-string
                           assignments-string))
    ;; (printf "\"SIG:\" ~a~n" signals-string)
    ;; (printf "\"ASS:\" ~a~n" assignments-string)
    )

  (define-syntax def-vhdl
    (syntax-rules ()
      [(_ name body ...)
       (parsed-code->vhdl
        (symbol->vhdl-symbol 'name) ;; replace all "-" with "_"
        (parse-code (list 'body ...)))]))
  (provide def-vhdl)


  )
