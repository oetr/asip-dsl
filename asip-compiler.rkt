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
