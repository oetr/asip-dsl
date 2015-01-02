;;; Chapter 4: Metalinguistic Abstraction
(define (ev exp env)
  (cond [(self-evaluating? exp)
         ;;(printf "self~n")
         exp]
        [(variable? exp)
          ;;(printf "variable: ~a~n" exp)
         (lookup-variable-value exp env)]
        [(quoted? exp)
          ;;(printf "quoted: ~a~n" exp)
         (text-of-quotation exp)]
        [(assignment? exp)
          ;;(printf "assignment: ~a~n" exp)
         (eval-assignment exp env)]
        [(definition? exp)
          ;;(printf "definition: ~a~n" exp)
         (eval-definition exp env)]
        [(if? exp)
          ;;(printf "if: ~a~n" exp)
         (eval-if exp env)]
        [(lambda? exp)
          ;;(printf "lambda~n")
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env)]
        [(begin? exp)
          ;;(printf "begin~n")
         (eval-sequence (begin-actions exp) env)]
        [(cond? exp)
          ;;(printf "cond~n")
         (ev (cond->if exp) env)]
        [(application? exp)
          ;;(printf "application~n")
         (appl (ev (operator exp) env)
               (list-of-values (operands exp) env))]
        [else
         (error "Unknown expression type -- EVAL " exp)]))

(define (appl procedure arguments)
  ;; (printf "proc: ~a; args: ~a~n" procedure arguments)
  (cond [(primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments)]
        [(compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure)))]
        [else
         (error "Unknown procedure type -- APPLY " procedure)]))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (ev (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (ev (if-predicate exp) env))
      (ev (if-consequent exp) env)
      (ev (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond [(last-exp? exps) (ev (first-exp exps) env)]
        [else (ev (first-exp exps) env)
              (eval-sequence (rest-exps exps) env)]))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (ev (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (ev (definition-value exp) env)
    env))

;; 4.1.2 Representing Expressions
(define (self-evaluating? exp)
  (cond [(number? exp) true]
        [(string? exp) true]
        [else false]))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (car (cdr exp)))

(define (tagged-list? exp tag)
  (cond [(mpair? exp)
         (eq? (mcar exp) tag)]
        [(pair? exp)
         (eq? (car exp) tag)]
        [else false]))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters
                   (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;; Derived expressions
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false ;; no else clause
      (let ([first (car clauses)]
            [rest (cdr clauses)])
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;; Testing of predicates
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;; Representing procedures
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;; Operations on Environments
(require racket/mpair)
(define (enclosing-environment env) (mcdr env))

(define (first-frame env) (mcar env))

(define the-empty-environment '())

(define (make-frame variables values)
  (mcons variables values))

(define (frame-variables frame) (mcar frame))

(define (frame-values frame) (mcdr frame))

(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (when (pair? vars)
    (set! vars (list->mlist vars)))
  (when (pair? vals)
    (set! vals (list->mlist vals)))
  (if (= (mlength vars) (mlength vals))
      (mcons (make-frame vars vals) base-env)
      (if (< (mlength vars) (mlength vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (mcar vars))
             (mcar vals)]
            [else (scan (mcdr vars) (mcdr vals))]))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (mcar vars))
             (set-mcar! vals val)]
            [else (scan (mcdr vars) (mcdr vals))]))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ([frame (first-frame env)])
    (define (scan vars vals)
      (cond [(null? vars)
             (add-binding-to-frame! var val frame)]
            [(eq? var (mcar vars))
             (set-mcar! vals val)]
            [else (scan (mcdr vars) (mcdr vals))]))
    (scan (frame-variables frame)
          (frame-values frame))))

;; 4.1.4 Running the Evaluator as a Program
(define (setup-environment)
  (let ([initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)])
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  ;; (printf "primitive-implementation ~a~n" proc)
  (mcar (mcdr proc)))

(define primitive-procedures
  (mlist (mlist 'car car)
         (mlist 'cdr cdr)
         (mlist 'cons cons)
         (mlist 'null? null?)
         (mlist '* *)
         (mlist '+ +)
         (mlist '- -)
         (mlist '= =)
         (mlist 'eval ev)
         (mlist 'apply appl)
         (mlist 'list list)))

(define (primitive-procedure-names)
  (mmap mcar
       primitive-procedures))

(define (primitive-procedure-objects)
  (mmap (lambda (proc) (mlist 'primitive (mcar (mcdr  proc))))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ([input (read)])
    (let ([output (ev input the-global-environment)])
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

(driver-loop)

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))
