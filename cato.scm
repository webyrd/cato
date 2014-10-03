(load "mk.scm")

(define appendo
  (lambda (l s out)
    (conde
      ((== '() l) (== s out))
      ((fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) out)
         (appendo d s res))))))

(define stepo
  (lambda (stack queue^ out)
    (fresh (word queue)
      (== `(,word . ,queue) queue^)
      (conde
        ((numbero word) (== `((,word . ,stack) ,queue) out))
        ((fresh (a d)
           (== `(,a . ,d) word)
           (== `(((,a . ,d) . ,stack) ,queue) out)))
        ((== 'dup word)
         (fresh (a rest)
           (== `(,a . ,rest) stack)
           (== `((,a . ,stack) ,queue) out)))
        ((== 'swap word)
         (fresh (a ad ddr)
           (== `(,a ,ad . ,ddr) stack)
           (== `((,ad . (,a . ,ddr)) ,queue) out)))
        ((== 'call word)
         (fresh (a d res)
           (== `(,a . ,d) stack)
           (== `(,d ,res) out)
           (appendo a queue res)))
        ((== 'cons word)
         (fresh (a d first rest)
           (== `(,first (,a . ,d) . ,rest) stack)
           (== `(((,first ,a . ,d) . ,rest) ,queue) out)))
        ((== 'drop word)
         (fresh (a d)
           (== `(,a . ,d) stack)
           (== `(,d ,queue) out)))
        ((fresh (quotation hide rest)
           (== 'dip word)
           (== `(,quotation ,hide . ,rest) stack)
           (fresh (a ad ddr res)
             (== `(,a ,ad . ,ddr) stack)
             (== `(,ddr ,res) out)
             (appendo quotation (cons hide queue) res))))))))

(define catwalko
  (lambda (queue out)
    (let loop ((stack '())
               (queue queue))
      (conde
        ((== '() queue) (== stack out))
        ((fresh (a d stack/queue s/q-a s/q-ad)
           (== `(,a . ,d) queue)
           (stepo stack queue stack/queue)
           (== `(,s/q-a ,s/q-ad) stack/queue)
           (loop s/q-a s/q-ad)))))))

(run* (q) (catwalko '(1 dup) q)) ; => ((1 1))
(run* (q) (catwalko '(1 2 swap) q)) ; => ((1 2))
(run* (q) (catwalko '((1) call) q)) ; => '((1))
(run* (q) (catwalko '(1 drop) q)) ; => '(())
(run* (q) (catwalko '(1 (2) dip) q)) ; => '((1 2))
(run* (q) (catwalko '((1) 2 cons) q)) ; => (((2 1)))

(run* (q) (catwalko '((dup cons) dup cons) q)) ; => ((((dup cons) dup cons)))
(run 1 (q) (catwalko q '(((dup cons) dup cons)))) ; => ((((dup cons) dup cons)))

(run 1 (q)
  (fresh (queue out a d stack)
    (== `(,a . ,d) q)
    (== `(,q call) queue)
    (== `(,q) stack)
    (catwalko queue stack)))
; generated a quine!
;
; => (((dup cons) dup cons))


(run 5 (q) (catwalko q '(1 1)))

(run 1 (q) (catwalko q q))

(run 1 (q)
  (fresh (queue out)
    (== `(,q call) queue)
    (catwalko queue q)))

#!eof

(run 1 (q)
  (fresh (queue out a d stack)
    (== `(,a . ,d) q)
    (== `(,q call) queue)
    (== `(,q) stack)
    (catwalko queue stack)))
