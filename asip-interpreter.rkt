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
 (def-reg a (from 10) (downto 0) (val 10))
 ;; looped signal definition
 
 ;; will be expanded into 10 signals/registers
 (def N 10)
 (def-regs a N (range 10 0))
 
 (def-reg a (range 10 0) (val 10))
 (def-wire b (range 10 0) (val 10))

 ;; register logic
 (when (rising-edge iCLK_50)
   (set a (0) (xor a (ref iKEY 0))))

 ;; combinatorial logic
 (set (b (downto 3 0)) (iKEY (downto 3 0)))
 (set (b (at 10 9 8 7))
      (iKEY (at 3 2 1 0)))

 ;; ASIP instructions
 (def-instruction (shift-regs)
   (set-regs (a 9 1) (a 8 0)))

 )




;; here is how user code will look like (python style):
;; def-i/o:
;;   iCLK_50 in
;;   iKEY    in  3  0
;;   oLEDR   out 17 0
;;   oLEDG   out 7  0
