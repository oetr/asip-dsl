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
                                     (list (gensym n)
                                           (lookup-width n))))
       (define arguments (cons 'name (map car arguments-and-width)))
       (define widths (append (flatten (reverse arguments-and-width))
                              (list closure-id OPERATION-WIDTH)))
       (eval `(define ,arguments
                (n->binary* ,INSTRUCTION-WIDTH . ,widths))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prepare for instruction generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define id (id-make))
(define instructions (instructions-make))
(reset-instructions!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-instruction (asip-set-rv reg val))
(define-instruction (asip-wait cycles))
(define-instruction (asip-jump line))
(define-instruction (asip-jump-if-true line))
(define-instruction (asip-add-rvr reg val reg))
(define-instruction (asip-eq-rvr reg val reg))
(define-instruction (asip-halt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(list
 (asip-set-rv 0 0)
 (asip-wait 50000000)
 (asip-add-rvr 0 1 0)
 (asip-eq-rvr 0 10 0)
 (asip-jump-if-true 6)
 (asip-jump 1)
 (asip-halt))
