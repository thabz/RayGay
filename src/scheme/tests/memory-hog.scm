
; Genbrug loop's frame til de nye variable istedet for at lave en
; ny frame hver gang, da loop lambda'en er lokal under let'en.

; Alle built-ins skal ikke returnere et result, men have et dest
; arg med. Dermed kan ovenstaaende udnyttes fuldt ud og vi
; kan koere med kun 100k allocations og ikke 1M.

(display
(reverse 
(let loop ((i 0)
           (l '()))
    (if (= i 100000)
        l
	(loop (+ 1 i) (cons i l))))))
(newline)    
 
