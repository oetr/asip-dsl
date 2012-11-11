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
     (begin
       (if (symbol? a-name)
           (racket-symbol->vhdl-symbol a-name)
           (racket-symbol->vhdl-symbol 'a-name)))]))

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
;;; Opcodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax macro-map
  (syntax-rules ()
    [(_ macro a-list)
     (map (lambda (arg)
            (macro arg))
          a-list)]))

(define-syntax convert-opcodes
  (syntax-rules ()
    [(_ opcodes)
     (~a
      "type opcodes is ("
      (apply (curry ~a #:separator ", ")
             (macro-map ->vhdl-name opcodes))
      ");\n")]))

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
   "  -- Opcodes"nl
   "  --------------------------------------------------------------------" nl
   (convert-opcodes (get-instructions))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define I/O ports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; assume it's always std-logic-vector
;; signal type is then defined by a list '(a . b)
;; -> std_logic_vector(a downto b);
(define (convert-io a-list)
  (printf "length of the list: ~a~n" a-list)
  (define type #f)
  (if (= 2 (length a-list))
      (set! type "std_logic")
      (set! type (~a "std_logic_vector(" (caddr a-list) " downto "
                     (cadddr a-list) ")")))
  (~a (car a-list) " : " (cadr a-list) " " type))

(define (define-ios io-list)
  (string-append (apply (curry ~a #:separator ";\n")
                       (map (lambda (one-io)
                              (convert-io one-io)) io-list))
                 "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate the main file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (generate-main-file i/o)
  (define nl "\n")
  (~a
   ;; make a header
   "library ieee;" nl
   "use ieee.std_logic_1164.all;" nl
   "use ieee.numeric_std.all;" nl
   "use work.instructions_lib.all;" nl
   "----------------------------------------------------------------------" nl
   "entity asip is" nl
   "port (" nl
   (define-ios i/o) ");" nl
   "end entity asip;" nl
   "----------------------------------------------------------------------" nl
   "architecture arch of asip is" nl
   "  --------------------------------------------------------------------" nl
   "  -- Signals"nl
   "  --------------------------------------------------------------------" nl
   "begin" nl
   "  --------------------------------------------------------------------" nl
   "  -- Rising edge process"nl
   "  --------------------------------------------------------------------" nl
   "  process (clk, reset) is" nl
   "  begin" nl
   "    if reset = '1' then" nl
   "      registers_reg    <= (others => (others => '0'));" nl

   ))

(printf (generate-main-file '((iCLK in))))

(display-to-file (generate-library-file mc) "test.vhd" #:exists 'replace)
