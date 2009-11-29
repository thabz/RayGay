;; 
;; Implementation of SRFI 48: Intermediate Format Strings
;; http://srfi.schemers.org/srfi-48/
;; ~a	Any	(display obj) for humans	yes
;; ~s	Slashified	(write obj) for parsers	yes
;; ~w	WriteCircular	(write-with-shared-structure obj) like ~s, but handles recursive structures	yes
;; ~d	Decimal	the obj is a number which is output in decimal radix	yes
;; ~x	heXadecimal	the obj is a number which is output in hexdecimal radix	yes
;; ~o	Octal	the obj is a number which is output in octal radix	yes
;; ~b	Binary	the obj is a number which is output in binary radix	yes
;; ~c	Character	the single charater obj is output by write-char	yes
;; ~y	Yuppify	the list obj is pretty-printed to the output	yes
;; ~?	Indirection	the obj is another format-string and the following obj is a list of arguments; format is called recursively	yes
;; ~K	Indirection	the same as ~? for backward compatability with some existing implementations	yes
;; ~[w[,d]]F	Fixed	~w,dF outputs a number with width w and d digits after the decimal; ~wF outputs a string or number with width w.	yes
;; ~~	Tilde	output a tilde	no
;; ~t	Tab	output a tab character	no
;; ~%	Newline	output a newline character	no
;; ~&	Freshline	output a newline character if it is known that the previous output was not a newline	no
;; ~_	Space	a single space character is output	no
;; ~h	Help	outputs one line of call synopsis, one line of comment, and one line of synopsis for each format directive, starting with the directive (e.g. "~t")	no

(define (format port format-string . objs)
 (let* ((par (open-string-output-port))
	(output-port (car par))
	(output-callback (cadr par))
	(eof (eof-object))
	(format-port (open-string-input-port format-string)))
  (let loop ((objs objs))
   (cond 
    ((port-eof? format-port) (output-callback))
    ((equal? (lookahead-char format-port) #\~) 
     (let ((ignored (get-char format-port))
           (c (get-char format-port)))
      (case c
       ((#\~) (put-char output-port c) 
	      (loop objs))
       ((#\d) (put-string output-port (number->string (car objs))) 
	      (loop (cdr objs)))
       ((#\x) (put-string output-port (number->string (car objs) 16)) 
	      (loop (cdr objs)))
       ((#\o) (put-string output-port (number->string (car objs) 8)) 
	      (loop (cdr objs)))
       ((#\b) (put-string output-port (number->string (car objs) 2)) 
	      (loop (cdr objs)))
       ((#\a) (put-datum output-port (car objs)) 
	      (loop (cdr objs))))))
    (else 
     (put-char output-port (get-char format-port))
     (loop objs))))))

