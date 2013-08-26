(load "terp.scm")

;programs/1.scm	programs/2.scm	programs/kmp.scm

(define (assert true?)
  (if (not true?)
      (error "Assertion failed")))

(define (print obj)
  (write obj)
  (newline))

(define (say . things)
  (for-each display things)
  (newline))

(define (snarf file)
  (call-with-input-file file
    (lambda (port)
      (let reading ()
	(let ((exp (read port)))
	  (if (eof-object? exp)
	      '()
	      (cons exp (reading))))))))

(define (test-program filename cases)
  (let ((program (snarf filename)))
    (for-each (lambda (test-case)
                (let ((expected (car test-case))
                      (function (caadr test-case))
                      (arguments (cdadr test-case)))
                  (let ((result (run program function arguments)))
                    (cond ((not (equal? expected result))
                           (say "Failed in " filename)
                           (display "For:      ") (print (cons function arguments))
                           (display "Expected: ") (print expected)
                           (display "Got:      ") (print result)
                           (newline))))))
              cases)))

(test-program 
 "programs/factor.scm"
 '((#t (prime? 2))
   (#t (prime? 3))
   (#f (prime? 4))
   (#t (prime? 41))
   (#f (prime? 42))
   ))

(test-program 
 "programs/power.scm"
 '((1  (power 3 0))
   (3  (power 3 1))
   (9  (power 3 2))
   (27 (power 3 3))
   ( 524288  (power 2 19))
   (1048576  (power 2 20))
   ))

(test-program 
 "programs/kmp.scm"
 '(
   (#t (search () ()))
   (#t (search () (a)))
   (#t (search (a b a b) (x a b c a b a c a b a b)))
   (#t (search (a b a b c) (x a b c a b a b a b c)))
   (#f (search (a b a b c) (x a b c a b a b a b d)))
   (#t (search (a b c a b c a c a b) (a b c a b c a b c a c a b d)))
   ))
