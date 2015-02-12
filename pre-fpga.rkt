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
 (reg-set a 30))


;; Go higher than that!
;; high-high-high-level programming in hardware
;; Sci-Fi level of programming
;; Programming of the future:
;; stick together large pieces of HW
;; the pieces configure themselves

;; concrete: go away from registers, low-level control
;; get things done
;; propagate information
;; ask for signal width later
;; get the problem solved
;; Case study: blob detection of an image (CCL)
;; pixel by pixel
;; keep track of current (x;y) coordinates
;; check neighbors
;; update connected components
(set pixel-status #t)

;; @rising edge?

(posedge
 (clk)
 (if reset
     (begin
       (1)
       (2)
       (3))
     (begin
       (1)
       (2))))

;; how about this?:
(when (rising-edge? clk)
  (when set?
    (set pixel-status #t)
    (set pixel-status #f))
  (if (= reset 0)
      (begin
        (set pixel-status #t)
        (set pixel-status #f))
      (begin
        (set a 0)
        (set b 0)
        (set c 0))))

(posedge (clk) (list sig1 sig2 sig3))

;; inverters
(def n 10)
(def inverters (range 0 n))

(for ([i n])
  (set inverters i (get inverters (mod (+ i 1) n))))


(def (pl a b c)
  (+ (* a b)
     (* b c)))


(set sig1 10)
(set (sig1 (+ sig2 3))
     (sig2 (+ 20 i))
     (sig8 (and (xor ) or ...)))

(set sig1 (+ a b c d))
(set sig1 (+ a b c d))

(set sig1 (xor (+ 10 a)
               (+ 10 b)))


()
(set sig1 (custom-multiply
           (+ 10 a)
           (+ 10 b)))

;; Save all pixels in a row buffer
(cond
 [(= y 0)
  (set (A B C) 0)]
 [(= x 0)
  (set ((A B) 0)
       (C row-buffer))]
 [(= x 1)
  (set ())]

(set C row-buffer)
(set B C)
(set A B)

(set A )
