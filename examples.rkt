;;#lang racket
(require racket
         rackunit
         "asip-interpreter.rkt")

(define ent1
  (def-vhdl lights-on
    (def-i/o ;; maybe def-interface
      ;; inputs and outputs
      (o (def oLEDR (range 10 0)))
      (o (def oLEDG (range 17 0))))
    (def a (range 0 10))
    (def b (range 0 10))
    (def c (range 0 10))
    (set a (+ a b))
    (set oLEDR 0 1)
    (set oLEDR 1 1)))

(display-to-file ent1 "lights_on.vhd"
                 #:exists 'replace)

;; Example app
(parse-code
 '(
   (def-i/o ;; maybe def-interface
     ;; inputs and outputs
     (i (def iCLK_50))
     (i (def iKEY (range 3 0)))
     (o (def oLEDR (range 17 0)))
     (o (def oLEDG (range 7 0)))
     (io (def GPIO_0 (range 31 0))))
   
   ;; registers, wires
   (def a (range 10 0) 10)
   (def ab (range 10 0) 10)
   (def b (range 10 0) 10)
   (def c (range 10 0) (others 0)) ;; set range and default values
   (def d (make-list 10 0)) ;; set default values only, derive range
   (def e (list 1 0 0 1 1 1 0))
   (def N 10) ;; constant
   
   ;; looped signal definition
   ;; will be expanded into 10 signals/registers
   (def-vector c N (range 10 0))

   ;; looped processing
   (for ([i (range 0 10)])
     (set a 0 (ref (* i 10) 0)))

   ;; register logic
   (when (rising-edge iCLK_50)
     (def temp (ref iKEY 0))
     (set c 0 (xor c temp)))

   (set c 10)

   ;; combinatorial logic
   (set b (range 3 0) (ref iKEY (range 3 0)))
   (set b (at 10 9 8 7) (ref iKEY (at 3 2 1 0)))

   ;; ASIP instructions
   (def (add-regs)
     (set c (+ c 10))
     (set b (+ c 20)))

   (def-asip a00
     (for ([i 0 10]) ;; come out as a counter
       (add-regs)
       (add-regs)))))

