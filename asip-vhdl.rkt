(module asip-vhdl racket
  (provide (all-defined-out))
  (require racket/format)
  
  (define nl "\n")
  (define vhdl-comment-delimiter 
    (~a 
     "------------------------------------------------------------"
     nl))
  
  (define (vhdl-standard-libraries)
    (~a vhdl-comment-delimiter
        "library ieee;" nl
        "use ieee.std_logic_1164.all;" nl
        "use ieee.numeric_std.all;" nl))
  
  (define (vhdl-entity name i/o (generics #f))
    (~a vhdl-comment-delimiter
        "entity " name " is" nl
        (if generics (~a generics nl) "")
        i/o nl
        "end entity " name ";" nl))
  
  (define (vhdl-architecture name signals assignments)
    (~a vhdl-comment-delimiter
        "architecture arch of " name " is" nl
        signals nl
        "begin" nl
        assignments nl
        "end architecture arch;" nl)))

