; Find all anagrams in a dictionary file
(define dict-filename "/usr/share/dict/words")

(define port (transcoded-port (open-file-input-port dict-filename) (native-transcoder)))

; Lowercase all chars and sort them to create the key
; Eq. "Tony" -> "noty"
(define (word->key word)
 (list->string (list-sort char<? (string->list (string-downcase word)))))

(define hash (make-hashtable string-hash equal?))

(let loop ((word (get-line port)))
 (if (not (eof-object? word))
  (let* ((key (word->key word)))
   (hashtable-set! hash key (cons word (hashtable-ref hash key '())))
   (loop (get-line port)))))

(display "Keys in hashtable: ")
(display (hashtable-size hash))
(newline)    

;   (if (> (length (hashtable-ref hash key '())) 2)
;    (begin
;      (display (hashtable-ref hash key "None"))
;      (newline)))
