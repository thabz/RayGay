; Find all anagrams in a dictionary file
(define dict-filename "/usr/share/dict/words")

(define port (transcoded-port (open-file-input-port dict-filename) (native-transcoder)))

; Lowercase all chars and sort them to create the key
; Eq. "Tony" -> "noty"
(define (word->key word)
 (list->string (list-sort char<? (map char-downcase (string->list word)))))

(define hash (make-hashtable string-hash equal?))

(let loop ((word (get-line port)))
 (if (not (eof-object? word))
  (let* ((key (word->key word)))
   (hashtable-set! hash key (cons word (hashtable-ref hash key '())))
   (if (> (length (hashtable-ref hash key '())) 2)
    (begin
      (display (hashtable-ref hash key "None"))
      (newline)))
   (loop (get-line port)))))

