;; Build the documentation
(define source-files (list
  "scenes/lib/colors.scm"
  "scenes/lib/vector-math.scm"
  "scenes/lib/handy-extensions.scm"
  "scenes/lib/raygay.scm"))  
  
(define (handle-source-file source-file output-html-file)
  (let ((f (open-file-input-port source-file #f #f (native-transcoder))))
    (do ((i 1 (+ i 1)))
      ((port-eof? f) (close-input-port f))
      (let ((datum (read f)))
        (if (and (not (eof-object? datum))
                 (list? datum)
                 (not (null? datum))
                 (equal? (car datum) 'define)
                 (list? (cadr datum))
                 (string? (caddr datum)))
            (begin
              (display (cadr datum))
              (newline)     
              (display (caddr datum))
              (newline)))))))

(let loop-files ((files source-files))
  (if (not (null? files))
    (begin
      (handle-source-file (car files) (string-append (car files) ".html"))
      (loop-files (cdr files)))))