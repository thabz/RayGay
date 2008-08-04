
; Splits a string into a list of string.
; (string-split #\space "abc def ghi") -> ("abc" "def" ghi")
; (string-split #\/ "1/2/3") -> ("1" "2" "3")
(define (string-split chr str)
 (let loop ((i 0))
  (cond
   ((= i (string-length str))
    (list str))
   ((char=? (string-ref str i) chr)
    (cons (substring str 0 i)
          (string-split chr (substring str (+ i 1) (string-length str)))))
   (else
    (loop (+ i 1))))))

(define (extract-vector lst)
 (list->vector (map string->number lst)))

(define (extract-face lst)
 (map 
  (lambda (ll) (map string->number ll))
  (map
   (lambda (part) (string-split #\/ part))
   lst)))

; This will load and parse a .obj file and return a mesh sceneobject.
; There a builtin function "load-obj" that is much faster.
(define (load-obj-scheme filename)
 (let ((ip (transcoded-port (open-file-input-port filename) 
                            (make-transcoder (latin-1-codec)))))
  (let lineloop ((vectors '()) 
		 (normals '())
		 (uvs '())
		 (faces '()))
   (if (port-eof? ip)
    (begin
      (display normals)(newline)
      (make-mesh vectors normals uvs faces))
    (let* ((line (get-line ip))
 	   (line-tokens (string-split #\space line))
 	   (cmd (car line-tokens))
 	   (args (cdr line-tokens)))
;     (display line)(newline)
     (cond
      ((equal? "v" cmd)
       (lineloop 
          (cons (extract-vector args) vectors)
          normals uvs faces))
      ((equal? "vt" cmd)
       (lineloop 
          vectors normals
          (cons (extract-vector args) uvs)
          faces))
      ((equal? "vn" cmd)
       (lineloop 
          vectors 
          (cons (extract-vector args) normals)
          uvs faces))
      ((equal? "f" cmd)
       (lineloop 
          vectors normals uvs
          (cons (extract-face args) faces)))
      (else
       (lineloop vectors normals uvs faces))))))))


