(module vhdl-abstractions racket
  (provide (all-defined-out))

  (define (vhdl-comment a-comment)
    (string-append "-- " a-comment))

  (define nl "\n")
  
  (define (vhdl-if condition yes no)
    (~a "if " condition " then" nl
        yes nl "else" nl no nl "end if;" nl))

  (define (vhdl-cond . args)
    (define condition 0)
    (define yes 1)
    (define no 2)
    (define main-part
      (for/list ([arg args]
                 [i (length args)])
        (cond [(= (modulo i 3) condition)
               (cond [(and (> i 2) (< i (- (length args) 3)))
                      (string-append "elsif " arg " then" nl)]
                     [(= i 0) (string-append "if " arg " then" nl)]
                     [else (string-append "else" nl)])]
              [(= (modulo i 3) yes) (string-append arg nl)]
              [else (string-append arg nl)])))
    (apply string-append
           (append main-part (list (string-append "end if;" nl)))))
  
  
  
  

  )
