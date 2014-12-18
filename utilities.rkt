(module utilities racket
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Number->instruction convertion
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; To convert a number into a string of binary digits
  ;; the length of the resulting string is equal to "bits"
  (define (n->binary a-number bits)
    (let ([binary-number (number->string a-number 2)])
      (string-append (make-string
                      (- bits (string-length binary-number)) #\0)
                     binary-number)))

  ;; To convert a set of numbers into a string of binary digits
  ;; the length of the resulting string is equal to "instruction-length"
  ;; if the total bit length of the provided args does not exceed
  ;; "instruction-length", the difference will be replaced with zeroes: "0"
  ;; Example: (n->binary* 10 3 2 0 2) -> "0000001100"
  ;; Example: (n->binary* 10 3 2 0 2 10 4) -> "0011001010"
  ;; Example: (n->binary* 10 3 2 0 2 10 4 0 3) -> error
  (define (n->binary* instruction-length . args)
    (when (not (zero? (modulo (length args) 2)))
      (error 'n->binary* "Number of arguments should be divisible by 2. ~a\n" args))
    ;; count the total bit length of the provided arguments
    ;; need to check every odd number in "args"
    (define all-length
      (let loop ([args args] [num #f])
        ;; xor toggles "num"
        (cond [(empty? args) 0]
              [num (+ (car args) (loop (cdr args) (not num)))]
              [else (loop (cdr args) (not num))])))
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
    ;; append zeroes from the left to match the
    ;; provided "instruction length"
    (define len (string-length result))
    (if (< len instruction-length)
        (string-append (build-string
                        (- instruction-length len)
                        (lambda (not-used) #\0)) result)
        result))


  ;; finds whether some of the elements in the list lof-elements
  ;; is in the a-list
  (define (find lof-elements . a-list)
    (cond [(empty? a-list) #f]
          [(ormap (lambda (element)
                    (equal? element (car a-list)))
                  lof-elements) #t]
          [(list? (car a-list))
           (or (apply find (cons lof-elements (car a-list)))
               (apply find (cons lof-elements (cdr a-list))))]
          [else (apply find (cons lof-elements (cdr a-list)))]))

  ;; given data widths, compute the resulting bit string and the
  ;; address ranges of each signal.
  ;; assume that the signals will be packed together
  ;; Example: (compute-ranges 1 2 5)
  ;; -> '((1 0 0) (2 1 2) (5 3 7))
  (define (compute-ranges . widths)
    (define start 0)
    (for/list ([element (in-list widths)])
      (define temp-start  start)
      (set! start (+ start element))
      (list element temp-start (- start 1))))

  (define (make-names . args)
    (for/list ([element (in-list args)])
      (define str (symbol->string element))
      (list element
            (string->symbol (string-append str"-low"))
            (string->symbol (string-append str"-hi")))))


  ;; the naming conventions are reg0---reg15, val, line, cycles
  ;; strips the name of
  (define (normalize-name a-symbol)
    (cond
     [(or (symbol=? a-symbol 'val)
          (symbol=? a-symbol 'line)
          (symbol=? a-symbol 'cycles)) a-symbol]
     ;; try to see whether the string is a register
     [else
      (define a-string (symbol->string a-symbol))
      (define string-root (regexp-match #px"[[:alpha:]]+"
                                        a-string))
      (when string-root
        (match (car string-root)
          ["reg" 'reg]
          [else
           (error
            'normalize-name
            "Name ~a does not follow the naming conventions~n"
            a-symbol)]))]))

  )
