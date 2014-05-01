(require racket/format)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Number->instruction convertion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To convert a number into a string of binary digits
;; the length of the resulting string is equal to "bits"
(define (n->binary a-number bits)
  (let ([binary-number (number->string a-number 2)])
    (string-append (make-string (- bits (string-length binary-number)) #\0)
                   binary-number)))

;; To convert a set of numbers into a string of binary digits
;; the length of the resulting string is equal to "instruction-length"
;; if the total bit length of the provided args does not exceed
;; "instruction-length", the difference will be replaced with zeroes: "0"
;; Example: (n->binary* 10 3 2 0 2) -> "0000000011"
;; Example: (n->binary* 10 3 2 0 2 10 4) -> "0010100011"
;; Example: (n->binary* 10 3 2 0 2 10 4 0 3) -> error
(define (n->binary* instruction-length . args)
  (when (not (zero? (modulo (length args) 2)))
    (error 'n->binary* "Number of arguments should be divisible by 2. ~a\n" args))
  ;; count the total bit length of the provided arguments
  ;; need to check every odd number in "args"
  (define all-length (let loop ([args args][num #f])
                       ;; xor toggles "num"
                       (cond [(empty? args) 0]
                             [num (+ (car args) (loop (cdr args) (xor num #t)))]
                             [else (loop (cdr args) (xor num #t))])))
  ;; total bit length larger than provided "instruction-length"
  (when (> all-length instruction-length)
    (error 'n->binary* "Exceeding instruction length ~a by ~a~n"
           instruction-length
           (- all-length instruction-length)))
  ;; apply n->binary on each pair in "args"
  (define result
    (apply string-append
           (let loop ([args args])
             (if (empty? args)
                 (list "")
                 (cons (n->binary (car args) (cadr args))
                       (loop (cddr args)))))))
  ;; append zeroes from the left to match the provided "instruction length"
  (define len (string-length result))
  (if (< len instruction-length)
      (string-append (build-string (- instruction-length len)
                                   (lambda (not-used) #\0))
                     result)
      result))


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
(define INSTRUCTION-WIDTH (+ OPERATION-WIDTH (* 3 REGISTER-N-WIDTH) REGISTER-WIDTH))


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

;; the naming conventions are r0---r15, val, line, cycles
;; strips the name of
(define (normalize-name a-symbol)
  (cond
   [(or (symbol=? a-symbol 'val)
        (symbol=? a-symbol 'line)
        (symbol=? a-symbol 'cycles)) a-symbol]
   ;; try to see whether the string is a register
   [else
    (define a-string (symbol->string a-symbol))
    (define string-root (regexp-match #px"[[:alpha:]]+" a-string))
    (when string-root
      (match (car string-root)
        ["reg" 'reg]
        [else (error
               'normalize-name
               "Name ~a does not follow the naming conventions~n" a-symbol)]))]))

;; finds whether some of the elements in the list lof-elements
;; is in the a-list
(define (find lof-elements . a-list)
  ;;(printf "~a, car: ~a, cdr: ~a~n" a-list (car a-list) (cdr a-list))
  (cond [(empty? a-list) #f]
        [(ormap (lambda (element)
                  (equal? element (car a-list)))
                lof-elements) #t]
        [(list? (car a-list)) (or (apply find (cons lof-elements (car a-list)))
                                  (apply find (cons lof-elements (cdr a-list))))]
        [else (apply find (cons lof-elements (cdr a-list)))]))

;; given a list of numbers, return a list of consed pairs for each number
;; the returned numbers specify the start and the end of each number
;; Example: (compute-range 1 2 3)
;; -> '((0 . 0) (1 . 2) (3 . 5))
(define (compute-range . args)
  (define start 0)
  (for/list ([element (in-list args)])
    (define temp-start  start)
    (set! start (+ start element))
    (list element temp-start (- start 1))))

(define (make-names . args)
  (for/list ([element (in-list args)])
    (define str (symbol->string element))
    (list element
          (string->symbol (string-append str"-low"))
          (string->symbol (string-append str"-hi")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro to define a procedure that is converted into a string
;; after it is run
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax define-instruction
  (syntax-rules (racket vhdl)
    [(_ (name . args) (racket body-r ...) (vhdl body-v ...))
     (begin
       (define closure-id (instruction-exists? 'name))
       (unless closure-id
         (set! closure-id id)
         (add-instruction! 'name id))
       ;; generate appropriate names
       (define arguments-and-width (for/list ([n (in-list 'args)])
                                     (list n (lookup-width (normalize-name n)))))
       (printf "test: ~a~n" arguments-and-width)
       (define arguments (map car arguments-and-width))
       (define all-widths (append (reverse arguments-and-width)
                                  (list (list closure-id OPERATION-WIDTH))))
       (define widths (append (flatten (reverse arguments-and-width))
                              (list closure-id OPERATION-WIDTH)))
       ;; prepare the name strings
       (define name-string (symbol->string 'name))
       ;;(printf "name-string: ~a~n" name-string)
       (define simulation-name
         (string->symbol (string-append name-string "-simulation")))
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
       (define manipulates-pc? (find '(pc-set! pc-increase!) 'body-r ...))
       (define definition-racket `(define (,simulation-name . ,arguments)
                                    body-r ...))
        (unless manipulates-pc?
          (set! definition-racket (append definition-racket `((,pc-increase!)))))
       ;; translate racket->vhdl
       (define ranges (flatten (apply compute-range (reverse (map cadr all-widths)))))
       (define names (flatten (apply make-names (cons 'op 'args))))
       (define definition-vhdl `(define ,vhdl-name
                                  (string-append
                                   (let ,(map list names ranges)
                                     (define nl "\n") ;; to not do it many times
                                     body-v ...)
                                   ,(if  manipulates-pc?
                                         "" '(increment-pc)))))
       (pretty-print definition-vhdl)
       (eval definition-vhdl)
       (eval definition-racket)
       )]))

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

(define (test-procedure)
  (define registers (vector 1 2 3))
  (r! 0 10))


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

(define (define-user-register name type (range1 #f) (range2 #f) (dir 'downto))
  (hash-set! user-registers name
             (make-register (define-type type range1 range2 dir) 0)))

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
  (~a "registers_next(" reg ") <= " new-value ";"))

(define (set-user-register name new-value)
  (~a name "_next <= " new-value ";"))

(define (get-user-register name)
  (~a name "_reg"))

(define (get-register reg)
  (~a "registers_reg(" reg ")"))

(define (increment-pc)
  "pc_next <= pc_reg + 1;\n")

(define (set-pc val)
  (~a "pc_next <= " val ";\n"))

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
;;; Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define user registers
(define-user-register 'counter 'integer) ;; 0 (sub1 (expt 2 REGISTER-WIDTH)) 'to)
(define-user-register 'started 'std_logic)
(define-user-register 'if 'std_logic)

;; define user operations
(define-instruction (asip-set-rv reg val)
  (racket
   (r! reg val))
  (vhdl
   (~a (set-register (get-i reg-hi reg-low) (get-s val-hi val-low)) nl)))


(define-instruction (asip-wait cycles)
  (racket
   (when (= (user-register-ref started) 1)
     (define c (user-register-ref counter))
     (if (or (= c 1) (= c 0))
         (begin
           (pc-increase!)
           (user-register-set! counter 0)
           (user-register-set! started 0))
         (user-register-set! counter (- c 1))))
   (when (= (user-register-ref started) 0)
     (user-register-set! started 1)
     (user-register-set! counter cycles)))
  (vhdl
   (~a
    "-- allow to wait for one clock cycle" nl
    "if " (get-i cycles-hi cycles-low) " = 1 then" nl
    (increment-pc)
    "elsif started_reg = '0' then" nl
    "counter_next <= "(get-i cycles-hi cycles-low) ";" nl
    "started_next      <= '1';" nl
    "else                            -- count down" nl
    "counter_next <= counter_reg - 1;" nl
    "if counter_reg = 0 then" nl
    "started_next <= '0';" nl
    (increment-pc)
    "end if;" nl
    "end if;" nl)))


(define-instruction (asip-jump line)
  (racket (pc-set! line))
  (vhdl (~a (set-pc (get-i line-hi line-low)))))


(define-instruction (asip-jump-if-true line)
  (racket
   (if (zero? (user-register-ref if))
       (pc-increase!)
       (pc-set! line)))
  (vhdl
   (~a
    "if " (get-user-register 'if) "= '0' then" nl
    (increment-pc)
    "else" nl
    (set-pc (get-i line-hi line-low))
    "end if;" nl)))

(define-instruction (asip-add-rvr reg val reg1)
  (racket
   (r! reg1 (+ (r$ reg) val)))
  (vhdl
   (~a
    (set-register (get-i reg1-hi reg1-low)
                  (i->s (~a (get-i val-hi val-low) " + "
                            (s->i (get-register (get-i reg1-hi reg1-low))))
                        val)) nl)))

(define-instruction (asip-eq-rv reg val)
  (racket
   (if (= (r$ reg) val)
       (user-register-set! if 1)
       (user-register-set! if 0)))
  (vhdl
   (~a "if " (get-register (get-i reg-hi reg-low)) " = "
       (get-s val-hi val-low) " then" nl
       (set-user-register 'if "'1'") nl
       "else" nl
       (set-user-register 'if "'0'") nl
       "end if;" nl)))


(define-instruction (asip-halt)
  (racket (pc-set! (pc-ref)))
  (vhdl (set-pc (get-pc))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mc
  (list
   (asip-set-rv-mc 0 0)
   (asip-wait-mc 50000000)
   (asip-add-rvr-mc 0 1 0)
   (asip-eq-rv-mc 0 10)
   (asip-jump-if-true-mc 6)
   (asip-jump-mc 1)
   (asip-halt-mc)))

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

(define sim1 (make-simulator
              (asip-set-rv-simulation 0 0)
              (asip-wait-simulation 1)
              (asip-add-rvr-simulation 0 1 0)
              (asip-eq-rv-simulation 0 10)
              (asip-jump-if-true-simulation 6)
              (asip-jump-simulation 1)
              (asip-halt-simulation)))

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

(simulator-run sim1 10 #t)
