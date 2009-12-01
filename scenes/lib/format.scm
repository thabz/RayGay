;; 
;; Implementation of SRFI 48: Intermediate Format Strings
;; http://srfi.schemers.org/srfi-48/
;;
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

(define (format . args)
 (define (put-padded port width obj)
  "Place obj as string within a space-padded field"
  (let* ((str (if (number? obj) (number->string obj) obj))
         (strw (string-length str)))
   (if (> strw width)
    (put-string port str)
    (let* ((sw (- width strw))
 	   (s (list->string (vector->list (make-vector sw #\space)))))
     (put-string port s)
     (put-string port str)))))
 (cond
  ((null? args) #f)
  ((string? (car args))
   (apply format (cons #f args)))
  ((eqv? #t (car args))
   (apply format (cons (current-output-port) (cdr args))))
  ((eqv? #f (car args))
   (let* ((par (open-string-output-port))
  	  (output-port (car par))
	  (output-callback (cadr par)))
    (apply format (cons output-port (cdr args)))
    (output-callback)))
  ((port? (car args))
   (let* ((output-port (car args))
	  (format-string (cadr args))
	  (objs (cddr args))
	  (format-port (open-string-input-port format-string)))
  (let loop ((objs objs))
   (cond 
    ((port-eof? format-port))
    ((equal? (lookahead-char format-port) #\~) 
     (let ((ignored (get-char format-port))
           (c (get-char format-port)))
      (case c
       ((#\~) (put-char output-port c) 
	      (loop objs))
       ((#\_) (put-char output-port #\space) 
	      (loop objs))
       ((#\%) (put-char output-port #\newline) 
	      (loop objs))
       ((#\t) (put-char output-port #\tab) 
	      (loop objs))
       ((#\d) (put-string output-port (number->string (car objs))) 
	      (loop (cdr objs)))
       ((#\x) (put-string output-port (number->string (car objs) 16)) 
	      (loop (cdr objs)))
       ((#\o) (put-string output-port (number->string (car objs) 8)) 
	      (loop (cdr objs)))
       ((#\b) (put-string output-port (number->string (car objs) 2)) 
	      (loop (cdr objs)))
       ((#\c) (put-char output-port (car objs))
	      (loop (cdr objs)))
       ((#\? #\K) (put-string output-port 
	       (apply format (cons #f (cons (car objs) (cadr objs))))) 
	      (loop (cddr objs)))
       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	      (let digit-loop ((digits (list c)))
	       (let ((d (get-char format-port)))
		(if (equal? d #\F)
		 (begin
		  (put-padded 
		   output-port
		   (string->number (list->string (reverse digits))) 
		   (car objs))
		  (loop (cdr objs)))
		 (digit-loop (cons d digits))))))
       ((#\s) (write (car objs) output-port)
	      (loop (cdr objs)))
       ((#\a) (put-datum output-port (car objs)) 
	      (loop (cdr objs))))))
    (else 
     (put-char output-port (get-char format-port))
     (loop objs))))))))

