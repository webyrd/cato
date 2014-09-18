(load "pmatch.scm")

(define step
  (lambda (stack queue)
    (let ((word (car queue))
          (queue (cdr queue)))
      (pmatch word
        [,n (guard (number? n)) `((,n . ,stack) ,queue)]
        [(,a . ,d) `(((,a . ,d) . ,stack) ,queue)]
        [dup `((,(car stack) . ,stack) ,queue)]
        [swap `((,(cadr stack) . (,(car stack) . ,(cddr stack))) ,queue)]
        [call `(,(cdr stack) ,(append (car stack) queue))]
        [drop `(,(cdr stack) ,queue)]
        [dip
         (let ((quotation (car stack))
               (hide (cadr stack)))
           `(,(cddr stack) ,(append quotation (cons hide queue))))]))))

(define run
  (lambda args
    (let loop ((stack '())
               (queue args))
      (cond
        ((null? queue) stack)
        (else
         (let ((stack/queue (step stack queue)))
           (loop (car stack/queue) (cadr stack/queue))))))))

(run 1 'dup) ; => (1 1)
(run 1 2 'swap) ; => (1 2)
(run '(1) 'call) ; => (1)
(run 1 'drop) ; => ()
(run 1 '(2) 'dip) ; => (1 2)
