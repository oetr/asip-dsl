(require racket/format)

(require "utilities.rkt")
(require "vhdl-abstractions.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Instructions have unique IDs
;;; Instruction symbols and their IDs are saved in a hash map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (instructions-make) (make-hash))

(define (instructions-add! instruction id)
  (hash-set! instructions instruction id))

(define (instructions-reset!)
  (set! instructions (instructions-make)))

(define (instruction-exists? instruction-name)
  (hash-ref instructions instruction-name #f))

(define (get-instructions)
  (hash-keys instructions))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ID will be associated with an instruction name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (id-make) 0)

(define (id-increase!)
  (set! id (+ id 1)))

(define (id-reset!)
  (set! id (id-make)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update free ID and
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (add-instruction! instruction-name instruction-id)
  (instructions-add! instruction-name instruction-id)
  (id-increase!))

(define (reset-instructions!)
  (instructions-reset!)
  (id-reset!))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prepare for instruction generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define id (id-make))
(define instructions (instructions-make))
(reset-instructions!)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define OPERATION-WIDTH   4)
(define REGISTER-WIDTH    32)
(define REGISTER-N-WIDTH  4)     ;; there are 2^4 registers
(define LINE-N-WIDTH      10)    ;; 1024 lines in code!
(define INSTRUCTION-WIDTH
  (+ OPERATION-WIDTH (* 3 REGISTER-N-WIDTH) REGISTER-WIDTH))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Store bit width of a key word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bit-widths
  (make-hash `((reg    . ,REGISTER-N-WIDTH)
               (val    . ,REGISTER-WIDTH)
               (line   . ,LINE-N-WIDTH)
               (cycles . ,REGISTER-WIDTH))))

;; To look up the width corresponding to a keyword
(define (lookup-width definition)
  (define result (hash-ref bit-widths definition #f))
  (unless result
    (error 'lookup-width "Width of ~a is unknown.~n" definition))
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define an instruction on several levels:
;; simulation
;; vhdl
;; Macro to define a procedure that is converted into a string
;; after it is run
;; allow 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax define-instruction
  (syntax-rules (racket vhdl)
    [(_ (name . args) (vhdl body-v ...))
     (define-instruction (name . args)
       (racket (begin)) (vhdl body-v ...))]
    [(_ (name . args) (racket body-r ...) (vhdl body-v ...))
     (begin
       (define closure-id (instruction-exists? 'name))
       (unless closure-id
         (set! closure-id id)
         (add-instruction! 'name id))
       ;; generate appropriate names
       (define arguments-and-width
         (for/list ([n (in-list 'args)])
           (list n (lookup-width (normalize-name n)))))
       (printf "arguments and width: ~a~n" arguments-and-width)
       (define arguments (map car arguments-and-width))
       (define all-widths (append (reverse arguments-and-width)
                                  (list (list closure-id
                                              OPERATION-WIDTH))))
       (define widths (append (flatten (reverse
                                        arguments-and-width))
                              (list closure-id OPERATION-WIDTH)))
       ;; prepare the name strings
       (define name-string (symbol->string 'name))
       ;;(printf "name-string: ~a~n" name-string)
       (define simulation-name
         (string->symbol (string-append name-string
                                        "-simulation")))
       (define machine-code-name
         (string->symbol (string-append name-string "-mc")))
       (define vhdl-name
         (string->symbol (string-append name-string "-vhdl")))
       ;; define appropriate procedures
       ;; procedure to convert the code into machine code
       (eval `(define (,machine-code-name . ,arguments)
                (n->binary* ,INSTRUCTION-WIDTH . ,widths)))
       ;; procedure to run the code in simulation
       ;; analyze the procedure's body to see if the
       ;; program counter gets incremented somewhere
       (define manipulates-pc? (find '(pc-set! pc-increase!)
                                     'body-r ...))
       (define definition-racket
         `(define (,simulation-name . ,arguments) body-r ...))
       (unless manipulates-pc?
         (set! definition-racket (append definition-racket
                                         `((,pc-increase!)))))
       ;; translate racket->vhdl
       (define ranges (flatten
                       (apply compute-ranges
                              (reverse (map cadr all-widths)))))
       (define names (flatten
                      (apply make-names (cons 'op 'args))))
       (define definition-vhdl `(define ,vhdl-name
                                  (string-append
                                   (let ,(map list names ranges)
                                     ;; to not do it many times
                                     (define nl "\n")
                                     body-v ...)
                                   ,(if  manipulates-pc?
                                         "" '(increment-pc)))))
       (pretty-print definition-vhdl)
       (eval definition-vhdl)
       (eval definition-racket))]))

;; -----------------------------------------------------------
;; Simulation environment
;; -----------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define registers (make-vector (expt 2 REGISTER-N-WIDTH) 0))

(define (registers-reset!)
  (set! registers (make-vector (expt 2 REGISTER-N-WIDTH) 0)))

(define (r! reg val)
  (vector-set! registers reg val))

(define (r$ reg)
  (vector-ref registers reg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-defined registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(struct register (type val) #:mutable #:transparent)
(define (make-register type val)
  (vector type val))

(define user-registers (make-hash))

(define (register-type a-reg) (vector-ref a-reg 0))
(define (register-val a-reg) (vector-ref a-reg 1))
(define (register-val-set! a-reg val) (vector-set! a-reg 1 val))
;;(define (register-val-ref name val) ((vector-ref a-reg 1))


(define (user-registers-reset!)
  ;; run over all registers and set the values to zero
  (hash-for-each user-registers
                 (lambda (name a-vector)
                   (register-val-set! a-vector 0))))

(define-syntax user-register-set!
  (syntax-rules ()
    [(_ name val)
     (register-val-set! (hash-ref user-registers 'name) val)]))

(define-syntax user-register-ref
  (syntax-rules ()
    [(_ name)
     (begin
       (unless (hash-ref user-registers 'name #f)
         (error "Define the type~n"))
       (register-val (hash-ref user-registers 'name)))]))

(define (define-type name (range1 #f) (range2 #f) (dir 'downto))
  (vector name range1 range2 dir))

(define (type-name a-type)   (vector-ref a-type 0))
(define (type-range1 a-type) (vector-ref a-type 1))
(define (type-range2 a-type) (vector-ref a-type 2))
(define (type-dir a-type)    (vector-ref a-type 3))

(define (define-user-register name type (range1 #f)
          (range2 #f) (dir 'downto))
  (hash-set! user-registers name
             (make-register (define-type type range1 range2 dir)
                            0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program counter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pc 0)
(define (pc-set! val) (set! pc val))
(define (pc-ref) pc)
(define (pc-increase!) (pc-set! (+ 1 (pc-ref))))
(define (pc-reset!) (set! pc 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clock cycle of the system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (clock-cycle-increase!)
  (set! current-clock-cycle
        (+ current-clock-cycle 1)))

(define (clock-cycle-reset!) (set! clock-cycle initial-clock-cycle))

(define initial-clock-cycle 0)

(define clock-cycle initial-clock-cycle)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reset simulation environment (but not the procedure
;;; definitions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (reset-environment!)
  (registers-reset!)
  (user-registers-reset!)
  (pc-reset!)
  (clock-cycle-reset!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions to make VHDL generation easier
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-i high low)
  (~a "get_i(" high", " low")"))

(define (get-u high low)
  (~a "get_u(" high", " low")"))

(define (get-s high low)
  (~a "get_s(" high", " low")"))

(define (set-register reg new-value)
  (~a "registers_reg(" reg ") <= " new-value ";"))

(define (set-user-register name new-value)
  (~a name "_reg <= " new-value ";"))

(define (get-user-register name)
  (~a name "_reg"))

(define (get-register reg)
  (~a "registers_reg(" reg ")"))

(define (increment-pc)
  "pc_reg <= pc_reg + 1;\n")

(define (set-pc val)
  (~a "pc_reg <= " val ";\n"))

(define (get-pc)
  "pc_reg")

(define (s->u val)
  (~a "unsigned(" val")"))

(define (u->i val)
  (~a "to_integer(" val")"))

(define (s->i val)
  (u->i (s->u val)))

(define (u->s val)
  (~a "std_logic_vector(" val")"))

(define (i->u val width)
  (~a "to_unsigned(" val ", " width")"))

(define (i->s val width)
  (u->s (i->u val width)))

;; convert to string
(define (s+ . args)
  (cond [(empty? args) ""]
        [(string? (car args))
         (string-append (car args)
                        (apply s+ (cdr args)))]
        [else (apply s+ (cdr args))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simulator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (execute-until-finished instruction)
  (define pc-before pc)
  (eval instruction)
  (define pc-after pc)
  (when (= pc-before pc-after) ;; execute again!
    (execute-until-finished instruction)))

(define-syntax make-simulator
  (syntax-rules ()
    [(_ instr1 ...)
     (let ([instr (vector 'instr1 ...)])
       (reset-environment!)
       (lambda (msg)
         (cond
          [(symbol=? msg 'step)
           (printf "~a~n" (vector-ref instr pc))
           (eval (vector-ref instr pc))])))]))

(define (simulator-step a-sim) (a-sim 'step))

(define (show-environment)
  (printf "PC:        ~a~n" pc)
  (printf "regs:      ~a~n" registers)
  (printf "user-regs: ")
  (pretty-print user-registers))

(define (simulator-run sim steps (debug #f))
  (when debug
    (printf "~n----- Simulation Start -------~n")
    (show-environment))
  (for ([step steps])
    (simulator-step sim1)
    (when debug
      (show-environment))))
