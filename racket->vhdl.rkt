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

(define (convert-constant name value (type "integer"))
  (if (string=? type "integer")
      (~a "constant " name " : " type " := " value ";\n")
      (~a "constant " name " : " type " := \"" value "\";\n")))

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
     (~a "subtype register_type is std_logic_vector("
         (->vhdl-name 'register-width)
         "-1 downto 0);\n"
         "type regs_type is array (natural range 0 to 2**"
         (->vhdl-name 'register-n-width)
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

(define-syntax define-opcodes
  (syntax-rules ()
    [(_ instructions)
     (let ([type (~a "std_logic_vector(" (->vhdl-name OPERATION-WIDTH)
                     "-1 downto 0)")])
       (apply ~a
              (hash-map instructions
                        (lambda (index value)
                          (convert-constant (->vhdl-name index)
                                            (n->binary value OPERATION-WIDTH)
                                            type)))))]))

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
   "------------------------------------------------------------" nl
   "package test is" nl
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
   (define-opcodes instructions)
   "  --------------------------------------------------------------------" nl
   "  -- Registers"nl
   "  --------------------------------------------------------------------" nl
   (convert-register-types REGISTER-WIDTH REGISTER-N-WIDTH) nl
   "  --------------------------------------------------------------------" nl
   "  -- Code"nl
   "  --------------------------------------------------------------------" nl
   (convert-machine-code mc) nl
   ;; finish library
   "end package test;"nl))

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

(define (convert-type type (range1 #f) (dir #f) (range2 #f))
  (define range #f)
  (when (and range1 dir range2)
    (set! range (~a range1 " " dir " " range2)))
  ;; check the type
  (match type
    ['integer (if range (~a type " range " range) (~a type))]
    ['std_logic_vector (~a type "(" range ")")]
    [_ (~a type)]))

(define (convert-signal name type (range1 #f) (range2 #f) (dir 'downto))
  (~a "signal " name " : " (convert-type type range1 dir range2) ";\n"))

(define (convert-register name type (range1 #f) (range2 #f) (dir 'downto))
  (convert-signal (string-append (~a (symbol->string name) "_reg"))
                  type range1 range2 dir))

;; registers is a hash map
(define (registers-rising-edge-default registers)
  (apply string-append
         (hash-map registers
                   (lambda (index value)
                     (define type (register-type (type-name value)))
                     (cond [(symbol=? type 'std_logic)
                            (~a index "_reg <= '0';\n")]
                           [(symbol=? type 'integer)
                            (~a index "_reg <= 0;\n")]
                           [else
                            (~a index "_reg <= (others <= '0');\n")])))))


(define (registers-assign-rising-edge registers (dir 'reg->next))
  (apply string-append
         (hash-map registers
                   (lambda (index value)
                     (~a index "_reg" " <= " index "_reg" ";\n")))))

(define (add-user-registers-to-comb-process user-registers)
  (define result 
    (apply (curry ~a #:separator ", ")
           (hash-map user-registers (lambda (index val)
                                      (string-append
                                       (symbol->string index) "_reg")))))
  (if (string=? result "")
      ""
      (string-append ", " result)))

(define (define-functions)
  "function get_i (
      constant hi : integer;
      constant low : integer)
      return integer is
    begin
        return to_integer(unsigned(instruction(hi downto low)));
    end function get_i;

    function get_s (
      constant hi : integer;
      constant low : integer)
      return std_logic_vector is
    begin
        return instruction(hi downto low);
    end function get_s;

    function get_u (
      constant hi : integer;
      constant low : integer)
      return unsigned is
    begin
        return unsigned(instruction(hi downto low));
    end function get_u;")

(define (symbol-append . symbols)
  (string->symbol
   (apply string-append (map symbol->string symbols))))

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
   "use work.test.all;" nl
   "------------------------------------------" nl
   "entity main is" nl
   "port (" nl
   (define-ios i/o) ");" nl
   "end entity main;" nl
   "------------------------------------------" nl
   "architecture arch of main is" nl
   "  ----------------------------------------" nl
   "  -- Signals"nl
   "  ----------------------------------------" nl
   "  signal reset, clk     : std_logic;" nl
   "  signal registers_reg  : regs_type := (others => (others => '0'));" nl
   (convert-register 'pc 'integer 0 (sub1 (expt 2 LINE-N-WIDTH)) 'to)
   (apply string-append (hash-map user-registers
                                  (lambda (name data)
                                    (define type (register-type data))
                                    (convert-register name
                                                      (type-name type)
                                                      (type-range1 type)
                                                      (type-range2 type)
                                                      (type-dir type)))))
   "begin" nl
   "  reset <= not iKEY(0);" nl
   "  clk   <= iCLK_50;" nl
   "  ----------------------------------------" nl
   "  -- Interpret instructions" nl
   "  ----------------------------------------" nl
   "  process (clk) is" nl
   "variable instruction      : std_logic_vector(" (->vhdl-name INSTRUCTION-WIDTH)
   "-1 downto 0);" nl
   "   variable op : std_logic_vector(" (->vhdl-name OPERATION-WIDTH)
   "-1 downto 0);" nl
   (define-functions) nl
   "  begin" nl
   ;;(registers-rising-edge-default user-registers)
   "    if rising_edge(clk) then" nl
   "-- Decode the operations" nl
   "instruction := instructions(pc_reg);" nl
   "op := instruction(" (->vhdl-name OPERATION-WIDTH) "-1 downto 0);" nl
   "-- Interpret operations" nl
   "case op is" nl
   (apply ~a (hash-map instructions
                       (lambda (index value)
                         (~a "when " (->vhdl-name index) " => " nl
                             (eval (symbol-append index '- 'vhdl)) nl))))
   "when others => null;" nl
   "end case;" nl
   ;;(registers-assign-rising-edge user-registers)
   "    end if;" nl
   "  end process;" nl
      "end architecture arch;" nl
   ))

