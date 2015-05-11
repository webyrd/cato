(define add
  (lambda (stack queue)
    (let ((n (car stack))
          (m (cadr stack)))
      (let ((sum (+ n m)))
        `((,sum . ,(cddr stack)) ,queue)))))

(define dup
  (lambda (stack queue)
    `((,(car stack) . ,stack) ,queue)))

(define swap
  (lambda (stack queue)
    `((,(cadr stack) . (,(car stack) . ,(cddr stack))) ,queue)))

(define call
  (lambda (stack queue)
    `(,(cdr stack) ,(append (car stack) queue))))

(define drop
  (lambda (stack queue)
    `(,(cdr stack) ,queue)))

(define dip
  (lambda (stack queue)
    (let ((quotation (car stack))
          (hide (cadr stack)))
      `(,(cddr stack) ,(append quotation (cons hide queue))))))

(define step
  (lambda (stack queue)
    (let ((word (car queue)))
      (if (procedure? word)
          (word stack (cdr queue))
          `(,(cons word stack) ,(cdr queue))))))

(define run
  (lambda args
    (let loop ((stack '())
               (queue args))
      (cond
        ((null? queue) stack)
        (else
         (let ((stack/queue (step stack queue)))
           (loop (car stack/queue) (cadr stack/queue))))))))

(define run-print
  (lambda args
    (let loop ((stack '())
               (queue args))
      (cond
        ((null? queue)
         (printf "stack: ~s\n" stack)
         (printf "queue: ~s\n" queue)
         (printf "----------------\n")
         stack)
        (else
         (printf "stack: ~s\n" stack)
         (printf "queue: ~s\n" queue)
         (printf "----------------\n")
         (let ((stack/queue (step stack queue)))
           (loop (car stack/queue) (cadr stack/queue))))))))


(run 1 dup)
(run 1 2 swap)
(run '(1) call)
(run 1 drop)
(run 1 '(2) dip)
