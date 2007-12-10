(define (table content)
  (string-append "<table>" content "</table>"))

(define (tr content)
  (string-append "<tr>" content "</tr>"))

(define (do-styles . styles)
  (if (null? styles) 
    ""
    (let loop ((result " style=\"")
	       (styles styles))
      (if (null? styles)
	(string-append result "\"")
	(loop (string-append result (caar styles) ":" (cadar styles) ";")
	      (cdr styles))))))


(define (td content . styles)
  (string-append "<td" (apply do-styles styles) ">" content "</td>"))

