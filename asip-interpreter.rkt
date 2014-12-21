;; Current problem with the macro solution:
;; two definitions of the same thing:
;; 1) simulator,
;; 2) conversion to vhdl code

;; Possible solution:
;; an interpreter to convert the DSL code into some form that can
;; be both: simulated and converted to VHDL based on user choice

;; Another way:
;; Two interpreters (or one interpreter and one compiler)
;; for the same code
;; 1) The first one simulated the code
;; 2) The second converts it to VHDL

;; The simulator should support concurrent execution
;; Should probably add delays for all standard operations

(define (sim-eval code)
  ;; traverse the code and find/merge:
  ;; ASIP description---ASIP instructions
  ;; ASIP code
  ;; signal/register definitions
  ;; external I/O that can be set by the user
  ;; returns a sorted list of all definitions
  (define (traverse-and-merge code)
    
    )

  ;; extract necessary state from the code
  ;; registers, initialize them
  
  )


;; Racket style code looks
(hw
 ;; inputs and outputs
 (def-i/o (list (iCLK_50 in)
                (iKEY in 3 0)
                (oLEDR out 17 0)
                (oLEDG out 7 0)))

 ;; registers, wires
 (def a (range 10 0) (val 10))
 (def b (range 10 0) (val 10))
 (def N 10) ;; constant
 
 ;; looped signal definition
 ;; will be expanded into 10 signals/registers
 (def-n c N (range 10 0))

 ;; register logic
 (when (rising-edge iCLK_50)
   (set c 0 (xor c (ref iKEY 0))))

 ;; combinatorial logic
 (set b (range 3 0) (iKEY (range 3 0)))
 (set b (at 10 9 8 7) (iKEY (at 3 2 1 0)))

 ;; ASIP instructions
 (def-instruction (shift-regs)
   (set-n (c 9 1) (c 8 0)))
 )

;; here is how user code will look like (python style):
;; def-i/o:
;;   iCLK_50 in
;;   iKEY    in  3  0
;;   oLEDR   out 17 0
;;   oLEDG   out 7  0
