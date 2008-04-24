
; Splits a string into a list of string.
; (string-split #\space "abc def ghi") -> ("abc" "def" ghi")
; (string-split #\/ "1/2/3") -> ("1" "2" "3")
(define (string-split chr str)
 (let* ((ip (open-string-input-port s))
        (pp (open-string-output-port))
	(op (car pp))
	(extract (cadr pp)))
  (let loop ((result '()))
   (cond 
    ((port-eof? ip) 
     (reverse result))
    ((eqv? (lookahead-char ip) c)
      (get-char ip)
      (loop (cons (extract) result)))
    (else
      (put-char op (get-char ip))
      (loop result))))))

; This will load and parse a .obj file and return a mesh sceneobject.
(define (load-obj filename)
 (let ((ip (open-file-input-port filename)))
  (let lineloop ((vectors '()) 
		 (normals '())
		 (uvs '())
		 (faces '()))
   (if (port-eof? ip)
    (make-mesh vectors normals uvs faces)
    (let* ((line (get-line ip))
 	   (line-tokens (string-split line))
 	   (cmd (car line-tokens))
 	   (args (cdr line-tokens)))
     (cond
      ((equal? "v" cmd))
      ((equal? "vt" cmd))
      ((equal? "vn" cmd))
      ((equal? "f" cmd))
      (else
       (lineloop vectors normals uvs faces))))))))


