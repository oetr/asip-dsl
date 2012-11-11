;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants->VHDL constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require racket/format)

;; to replace all occurrences of "-" to "_"
(define (racket-symbol->vhdl-symbol a-symbol)
  (string->symbol (string-replace (symbol->string a-symbol) "-" "_")))

;; no need to type "'" before converting the name of the constant
;; but it still works if oyu do!
(define-syntax ->vhdl-name
  (syntax-rules ()
    [(_  a-name)
     (if (symbol? a-name)
         (racket-symbol->vhdl-symbol a-name)
         (racket-symbol->vhdl-symbol 'a-name))]))

(define (convert-constant name value)
  (if (number? value)
      (~a "constant " name " : integer := " value ";\n")
      (error "the value of the constant is not a number~n")))

(define-syntax convert-constants*
  (syntax-rules ()
    [(_  name1 ...)
     (~a (convert-constant (racket-symbol->vhdl-symbol 'name1) name1)
         ... )]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Register definitons -> VHDL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax convert-register-types
  (syntax-rules ()
    [(_ register-width register-n-width)
     (~a "subtype register_type is std_logic_vector(" (->vhdl-name 'register-width)
      "-1 downto 0);\n"
      "type regs_type is array (natural range 0 to 2**" (->vhdl-name 'register-n-width)
      "-1) of\n register_type;\n")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Machine Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (convert-machine-code mc)
  (define INSTRUCTION-COUNT (length mc))
  (define constant-string (convert-constants* INSTRUCTION-COUNT))
  ;; declare a type to represent instructions
  (define type-string
    (~a "type instruction_rom_type is array (0 to "
        (->vhdl-name INSTRUCTION-COUNT) " - 1) of\n"
        "std_logic_vector (" (->vhdl-name INSTRUCTION-WIDTH)  "-1 downto 0);\n"))
  (define machine-code
    (~a
     "constant INSTRUCTIONS : instruction_rom_type :=\n(\""
     (apply (curry ~a #:separator "\",\n\"") mc)
     "\");\n"))
  ;; result
  (~a constant-string
      type-string
      machine-code))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Machine Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (generate-library-file mc)
  (define nl "\n")
  (~a
   ;; make a header
   "library ieee;" nl
   "use ieee.std_logic_1164.all;" nl
   "use ieee.numeric_std.all;" nl
   "----------------------------------------------------------------------" nl
   "package instructions_lib is" nl
   "  --------------------------------------------------------------------" nl
   "  -- Constants"nl
   "  --------------------------------------------------------------------" nl
   (convert-constants* OPERATION-WIDTH
                       REGISTER-WIDTH
                       REGISTER-N-WIDTH
                       LINE-N-WIDTH
                       INSTRUCTION-WIDTH) nl
   "  --------------------------------------------------------------------" nl
   "  -- Registers"nl
   "  --------------------------------------------------------------------" nl
   (convert-register-types REGISTER-WIDTH REGISTER-N-WIDTH) nl
   "  --------------------------------------------------------------------" nl
   "  -- Code"nl
   "  --------------------------------------------------------------------" nl
   (convert-machine-code mc) nl
   ;; finish library
   "end package instructions_lib;"nl))


(display-to-file (generate-library-file mc) "test.vhd" #:exists 'replace)
