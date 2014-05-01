;; circuit language
;; input--output

;; registers

;; mux


;; Wires
(define (make-wire . ports)
  ports)

(define (connect-port-wire port wire)
  )

(define (disconnect-port-wire port wire)
  )

;; disconnect port from wire


;; high-level code to FPGA


;; Racket code on Racket VM



;; Scheme-79 VM
;; take a scheme expression and interpret it
;; map it into the instructions on the VM

;; SICP interpreter



;; VHDL from Racket
;; Parallelize VM instructions---lay out some of them in hardware
;; Code analysis: dependencies in the code


;; !ASIP!
;; Not interested in VM, because it sucks!!!
;; Don't want to change stuff at the runtime
;; Fixed hardware!


;; use dedicated softcore whenever possible

;; register definition
;; automatic type derivation
;; define-reg <name> <bits> <initial value>
(define-reg a 10 1)
(define-reg b 10 0)
(define-reg c 10 0)

(define-reg (a 10 0) (b 20 0) (c 100 0))

;; connect to I/O
(define-i (iCLK_50 1 0) (iKEY 4 0))
(define-o (oLEDR 18) (oLEDG 8))
(define-z (ethernet 10))

(asip-cond [(@rising iCLK_50)
            ]
           [(@rising iCLK_50)])

(process
 (reg-set a 20)
 (reg-set a 30)

