(module asip-vhdl racket
  (provide (all-defined-out))
  (require racket/format)
  
  (define (vhdl-entity name i/o (generics #f))
    (define nl "\n")
    (~a "entity " name " is" nl
        (if generics (~a generics nl) "")
        i/o nl
        "end entity " name ";" nl))
  
  )

