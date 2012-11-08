;;; Constants
(define INSTRUCTION_NAME_WIDTH 4)
(define REG_WIDTH 32)
(define REG_COUNT_BITS 4)
(define notINSTRUCTION_NAME_WIDTH (+ REG_WIDTH REG_COUNT_BITS))
(define WHOLE_INSTRUCTION_WIDTH (+ REG_WIDTH REG_COUNT_BITS INSTRUCTION_NAME_WIDTH))

;;; ----------------------------------------
;;; Low-level instructions
;;; ----------------------------------------
(define (asip-set register value)
  (define id 0)
  (n->binary* WHOLE_INSTRUCTION_WIDTH
              REG_WIDTH               value
              REG_COUNT_BITS          register
              INSTRUCTION_NAME_WIDTH  id))

(define (asip-wait cycles)
  (define id 1)
  (n->binary* 1 notINSTRUCTION_NAME_WIDTH cycles
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; New approach that abstracts away a lot of things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (n->binary bits a-number)
  (let ([binary-number (number->string a-number 2)])
    (string-append (make-string (- bits (string-length binary-number)) #\0)
                   binary-number)))

(define (n->binary* instruction-length . args)
  (when (not (zero? (modulo (length args) 2)))
    (error 'n->binary* "Number of arguments should be divisible by 2. ~a\n" args))
  (define all-length (let loop ([args args][num #t])
                       (cond [(empty? args) 0]
                             [else (if num
                                       (+ (car args) (loop (cdr args) (xor num #t)))
                                       (loop (cdr args) (xor num #t)))])))
  (when (> all-length instruction-length)
    (error 'n->binary* "Exceeding instruction length ~a by ~a~n"
           instruction-length
           (- all-length instruction-length)))
  (define result
    (apply string-append
           (let loop ([args args])
             (if (empty? args)
                 (list "")
                 (cons
                  (n->binary (car args) (cadr args))
                  (loop (cddr args)))))))
  (define len (string-length result))
  (if (< len instruction-length)
      (string-append (build-string (- instruction-length len)
                                   (lambda (not-used) #\0))
                     result)
      result))

(define OPERATION-WIDTH 4)
(define REGISTER-WIDTH 32)
(define REGISTER-N-WIDTH 4) ;; there are 2^4 registers
(define LINE-N-WIDTH 10) ;; 1024 lines in code!
(define INSTRUCTION-WIDTH (+ OPERATION-WIDTH (* 3 REGISTER-N-WIDTH) REGISTER-WIDTH))

;; represent instructions as a list
(define (instructions-make) (make-hash))

(define (instructions-add! instruction id)
  (hash-set! instructions instruction id))

(define (instructions-reset!)
  (set! instructions (instructions-make)))

(define (instruction-exists? instruction-name)
  (hash-ref instructions instruction-name #f))


;; represent the id
(define (id-make) 0)

(define (id-increase!)
  (set! id (+ id 1)))

(define (id-reset!)
  (set! id (id-make)))


;; setting and resetting instructions and the current id
(define (add-instruction! instruction-name instruction-id)
  (instructions-add! instruction-name instruction-id)
  (id-increase!))

(define (reset-instructions!)
  (instructions-reset!)
  (id-reset!))

(define id (id-make))

(define instructions (instructions-make))

(define bit-widths
  (make-hash `((reg    . ,REGISTER-N-WIDTH)
               (val    . ,REGISTER-WIDTH)
               (line   . ,LINE-N-WIDTH)
               (cycles . ,REGISTER-WIDTH))))

(define (lookup-width definition)
  (define result (hash-ref bit-widths definition #f))
  (unless result
    (error 'lookup-width "Width of ~a is unknown.~n" definition))
  result)


(define-syntax define-instruction
  (syntax-rules ()
    [(_ name . args)
     (begin
       (define closure-id (instruction-exists? 'name))
       (unless closure-id
         (set! closure-id id)
         (add-instruction! 'name id))
       ;; generate appropriate names
       (define arguments-and-width (for/list ([n (in-list 'args)])
                                     (list (lookup-width n)
                                           (gensym n))))
       (define arguments (cons 'name (map cadr arguments-and-width)))
       (define widths (append (flatten (reverse arguments-and-width))
                              (list OPERATION-WIDTH  closure-id)))
       (eval `(define ,arguments
                (n->binary* ,INSTRUCTION-WIDTH . ,widths))))]))

(reset-instructions!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-instruction asip-set-rv reg val)
(define-instruction asip-wait cycles)
(define-instruction asip-jump line)
(define-instruction asip-jump-if-true line)
(define-instruction asip-add-rvr reg val reg)
(define-instruction asip-eq-rvr reg val reg)
(define-instruction asip-halt)

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
