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
    (vhdl-comment "allow to wait for a bunch of clock cycles") nl
    "if " (get-i cycles-hi cycles-low) " = 1 then" nl
    (increment-pc)
    "elsif started_reg = '0' then" nl
    "counter_reg <= "(get-i cycles-hi cycles-low) ";" nl
    "started_reg      <= '1';" nl
    "else" (vhdl-comment "count down") nl
    "counter_reg <= counter_reg - 1;" nl
    "if counter_reg = 0 then" nl
    "started_reg <= '0';" nl
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
                            (s->i (get-register
                                   (get-i reg1-hi reg1-low))))
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
;;; Simulate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sim1 (make-simulator
              (asip-set-rv-simulation 0 0)
              (asip-wait-simulation 1)
              (asip-add-rvr-simulation 0 1 0)
              (asip-eq-rv-simulation 0 10)
              (asip-jump-if-true-simulation 6)
              (asip-jump-simulation 1)
              (asip-halt-simulation)))

(simulator-run sim1 3 #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Save to a file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display-to-file (generate-main-file '((iCLK_50 in)
                                       (iKEY in 3 0)
                                       (oLEDR out 17 0)
                                       (oLEDG out 7 0)))
                 "main.vhd" #:exists 'replace)

(display-to-file (generate-library-file mc) "test.vhd" #:exists 'replace)

