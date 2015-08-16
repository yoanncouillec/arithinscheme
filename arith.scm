(module arith)

(define (evaluate expression)
   (match-case expression
      ((? number?)
       expression)
      ((+ ?a1 ?a2)
       (+ (evaluate a1) (evaluate a2)))
      ((- ?a1 ?a2)
       (- (evaluate a1) (evaluate a2)))))

(define (compile expression)
   (match-case expression
      ((? number?)
       `((CONST ,expression)))
      ((+ ?a1 ?a2)
       `(,@(compile a1) ,@(compile a2) ADD))
      ((- ?a1 ?a2)
       `(,@(compile a1) ,@(compile a2) SUB))))

(define (exec code stack)
   (if (pair? code)
       (match-case (car code)
	  ((CONST ?n)
	   (exec (cdr code) (cons n stack)))
	  (ADD
	   (exec (cdr code) (cons (+ (cadr stack) (car stack)) (cddr stack))))
	  (SUB
	   (exec (cdr code) (cons (- (cadr stack) (car stack)) (cddr stack)))))
       (car stack)))

(define e '(- 5 (+ 1 2)))
(print e)
(print "Evaluate = " (evaluate e))
(print "Execute =  " (exec (print "Compile = " (compile e)) '()))
