


; Renders 'a b as string a="b"
(define (do-attribute attr value)
  (string-append " " attr "=\"" value "\""))

(define (join-string-list l)
  (if (null? l) ""
  (string-append (car l) (join-string-list (cdr l)))))

(define (do-tag-with-attributes tag . content)
   (string-append "<" tag (do-attributes attributes) ">"
                  (join-string-list content) 
                  "</" tag ">"))

(define (do-tag tagname . args)
   (define (render . args)
      (cond 
         ((null? args) ">")
         ((symbol? (car args)) 
            (string-append " " (render (car args) (cadr args))))))
   (string-append "<" tagname (render args) "</" tagname ">"))

(define (table a) (do-tag "table" a))

; Renders '((a b)(c d)) as string 'a:b;c:d'
(define (do-styles . styles)
  (if (null? styles) 
    ""
    (let loop ((result "")
	       (styles styles))
      (if (null? styles)
        result
	(loop (string-append result (caar styles) ":" (cadar styles) ";")
	      (cdr styles))))))


(define (td content . styles)
  (string-append "<td" (apply do-styles styles) ">" content "</td>"))


