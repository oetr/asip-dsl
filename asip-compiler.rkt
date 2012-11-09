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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro to define a procedure that is converted into a string
;; after it is run
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax define-instruction
  (syntax-rules ()
    [(_ (name . args) body ...)
     (begin
       (define closure-id (instruction-exists? 'name))
       (unless closure-id
         (set! closure-id id)
         (add-instruction! 'name id))
       ;; generate appropriate names
       (define arguments-and-width (for/list ([n (in-list 'args)])
                                     (list n (lookup-width (normalize-name n)))))
       (define arguments (map car arguments-and-width))
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
       (define manipulates-pc? (find '(pc-set! pc-increase!) 'body ...))
       (define definition `(define (,simulation-name . ,arguments)
                             body ...))
       (unless manipulates-pc?
         (set! definition (append definition `((,pc-increase!)))))
       (eval definition)
       (pretty-print definition)
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
(define user-registers (make-hash))

(define (user-registers-reset!)
  (set! user-registers (make-hash)))

(define (user-register-set! reg val)
  (hash-set! user-registers reg val))

(define (user-register-ref reg)
  (hash-ref user-registers reg #f))

(define (define-user-register name initial-value)
  (hash-set! user-registers name initial-value))

(define-syntax define-user-register
  (syntax-rules ()
    [(_ name)
     (begin
       (define name 'name)
       (unless (user-register-ref 'name)
         (hash-set! user-registers 'name 0)))]))

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
;;; Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-instruction (asip-set-rv reg val)
  (r! reg val))

(define-instruction (asip-wait cycles)
  (define-user-register counter)
  (define-user-register started)
  (when (= (user-register-ref started) 0)
    (user-register-set! started 1)
    (user-register-set! counter cycles))
  (when (= (user-register-ref started) 1)
    (define c (user-register-ref counter))
    (if (or (= c 1) (= c 0))
        (begin
          (pc-increase!)
          (user-register-set! counter 0)
          (user-register-set! started 0))
        (user-register-set! counter (- c 1)))))

(define-instruction (asip-jump line)
  (pc-set! line))

(define-instruction (asip-jump-if-true line)
  
  )
(define-instruction (asip-add-rvr reg val reg1))

(define-instruction (asip-eq-rvr reg val reg1)
  (if (= reg val)
      (r! reg1 1)
      (r! reg1 0)))

(define-instruction (asip-halt)
  (pc-set! (pc-ref)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(list
 (asip-set-rv-mc 0 0)
 (asip-wait-mc 50000000)
 (asip-add-rvr-mc 0 1 0)
 (asip-eq-rvr-mc 0 10 0)
 (asip-jump-if-true-mc 6)
 (asip-jump-mc 1)
 (asip-halt-mc))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simulator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (execute-until-finished instruction)
  (printf "executing: ~a, pc: ~n" instruction)
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
           (printf "instr: ~a~n" (vector-ref instr pc))
           (eval (vector-ref instr pc))])))]))

(define sim1 (make-simulator
              (asip-set-rv-simulation 0 20)
              (asip-set-rv-simulation 1 1)
              (asip-set-rv-simulation 2 2)
              (asip-set-rv-simulation 3 3)
              (asip-set-rv-simulation 4 4)
              (asip-wait-simulation 10)
              (asip-halt-simulation)))

(define (simulator-step a-sim) (a-sim 'step))

(simulator-step sim1) registers
