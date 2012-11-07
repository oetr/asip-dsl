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
  (printf "jump-line: ~a~n" line)
  (define id 2)
  (n->binary* notINSTRUCTION_NAME_WIDTH line
              INSTRUCTION_NAME_WIDTH    id))

(define (asip-jump-if-zero line)
  (define id 3)
  (printf "jifz-line: ~a~n" line)
  (n->binary* notINSTRUCTION_NAME_WIDTH line
              INSTRUCTION_NAME_WIDTH    id))

(define (asip-halt)
  (define id (- (expt 2 INSTRUCTION_NAME_WIDTH) 1))
  (n->binary* notINSTRUCTION_NAME_WIDTH 0
              INSTRUCTION_NAME_WIDTH    id))

(define (asip-copy-register reg1 reg2) ;; copy reg1 -> reg2
  (define id 4)
  (n->binary* (- notINSTRUCTION_NAME_WIDTH (* 2 REG_COUNT_BITS)) 0
              REG_COUNT_BITS reg2
              REG_COUNT_BITS reg1
              INSTRUCTION_NAME_WIDTH id))

(define (call-standard reg val id)
  (n->binary* REG_WIDTH              val
              REG_COUNT_BITS         reg
              INSTRUCTION_NAME_WIDTH id))

;; compare register with a value (write result into a temporary register)
(define (asip-r=v? reg val)
  (define id 5)
  (call-standard reg val id))

(define (asip-r=r? reg0 reg1)
  (define id 6)
  (call-standard reg0 reg1 id))

(define (asip-r<v? reg val)
  (define id 7)
  (call-standard reg val id))

(define (asip-r<r? reg0 reg1)
  (define id 8)
  (call-standard reg0 reg1 id))

(define (asip-r<=v? reg val)
  (define id 9)
  (call-standard reg val id))

(define (asip-r<=r? reg0 reg1)
  (define id 10)
  (call-standard reg0 reg1 id))

(define (asip+rv reg val)
  (define id 11)
  (call-standard reg val id))

(define (asip-jmp-if-true line)
  (printf "jump-if-true: ~a~n" line)
  (define id 12)
  (n->binary* notINSTRUCTION_NAME_WIDTH line
              INSTRUCTION_NAME_WIDTH    id))

(define (asip-jmp-if-false line)
  (printf "jump-if-false: ~a~n" line)
  (define id 13)
  (n->binary* notINSTRUCTION_NAME_WIDTH line
              INSTRUCTION_NAME_WIDTH    id))

(define (asip-jmp-if-r=v? line)
  (printf "jump-if=r-v?: ~a~n" line)
  (define id 14)
  (n->binary* notINSTRUCTION_NAME_WIDTH line
              INSTRUCTION_NAME_WIDTH    id))

;;; ----------------------------------------
;;; High-level instructions
;;; ----------------------------------------
(define-syntax asip-while
  (syntax-rules (line)
    [(_ (line start-i) condition body ...)
     (let ([body+rest (list body ... (asip-jump start-i))])
       (printf "start: ~a, length: ~a~n" start-i (length body+rest))
       (append (list condition
                     (asip-jump-if-zero (+ 2 start-i (length body+rest))))
               body+rest
               ))]
    [(_ condition body ...)
     (asip-while (line 0) condition body ...)]))

;; macro to generate machine code from the instructions
(define-syntax mc-generate
  (syntax-rules (asip-while)
    [(_ (asip-while condition body ...) instructions ...)
     (let ([res (asip-while (line i) condition body ...)])
       (set! i (+ i (length res)))
       (append res
               (mc-generate instructions ...)))]
    [(_) '()]
    [(_ fn0 fn1 ...)
     (begin
       (set! i (+ i 1))
       (cons fn0
             (mc-generate fn1 ...)))]))

;; a test program
(define i 0)
(list
 (asip-set 0 0)
 (asip-wait 50000000)
 (asip+rv 0 1)
 (asip-r=v? 0 10)
 (asip-jmp-if-true 6)
 (asip-jump 1)
 (asip-halt))
