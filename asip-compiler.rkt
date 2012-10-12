;;; Constants
(define INSTRUCTION_NAME_WIDTH 4)
(define REG_WIDTH 32)
(define REG_COUNT_BITS 4)
(define notINSTRUCTION_NAME_WIDTH (+ REG_WIDTH REG_COUNT_BITS))
(define WHOLE_INSTRUCTION_WIDTH (+ REG_WIDTH REG_COUNT_BITS INSTRUCTION_NAME_WIDTH))

;;; Low-level operations
(define (n->binary bits a-number)
  (let ([binary-number (number->string a-number 2)])
    (string-append (make-string (- bits (string-length binary-number)) #\0)
                   binary-number)))

;; TODO: figure out how to use macro in this case:
;; (define-syntax n->binary*
;;   (syntax-rules ()
;;     [(n->binary* bits a-number)
;;      (n->binary bits a-number)]
;;     [(n->binary* bits0 a-number0 bits1 a-number1 ...)
;;      (string-append
;;       (n->binary bits0 a-number0)
;;       (n->binary bits1 a-number1)
;;       ...)]))

(define (n->binary* . args)
  (when (not (zero? (modulo (length args) 2)))
    (error 'n->binary* "Number of arguments should be divisible by 2.\n"))
  (apply string-append
         (let loop ([args args])
           (if (empty? args)
               (list "")
               (cons
                (n->binary (car args) (cadr args))
                (loop (cddr args)))))))

;;; ----------------------------------------
;;; Low-level instructions
;;; ----------------------------------------
(define (asip-set register value)
  (define id 0)
  (n->binary* REG_WIDTH               value
              REG_COUNT_BITS          register
              INSTRUCTION_NAME_WIDTH  id))

(define (asip-wait cycles)
  (define id 1)
  (n->binary* notINSTRUCTION_NAME_WIDTH cycles
              INSTRUCTION_NAME_WIDTH    id))

(define (asip-jump line)
  (define id 2)
  (n->binary* notINSTRUCTION_NAME_WIDTH line
              INSTRUCTION_NAME_WIDTH    id))

(define (asip-halt)
  (define id (- (expt 2 INSTRUCTION_NAME_WIDTH) 1))
  (n->binary* notINSTRUCTION_NAME_WIDTH 0
              INSTRUCTION_NAME_WIDTH    id))

(define (asip-copy-register reg1 reg2) ;; copy reg1 -> reg2
  (define id 3)
  (n->binary* (- notINSTRUCTION_NAME_WIDTH (* 2 REG_COUNT_BITS)) 0
              REG_COUNT_BITS reg2
              REG_COUNT_BITS reg1
              INSTRUCTION_NAME_WIDTH id))


;; tests
(asip-halt)
(asip-set 0 10)
(asip-wait 10000000000)
(asip-jump 4)
(asip-copy-register 0 1)

;;; ----------------------------------------
;;; Low-level instructions
;;; ----------------------------------------


