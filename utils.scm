(define (snarf file)
  (call-with-input-file file
    (lambda (port)
      (let reading ()
	(let ((exp (read port)))
	  (if (eof-object? exp)
	      '()
	      (cons exp (reading))))))))

(define (print obj)
  (write obj)
  (newline))

(define (say . things)
  (for-each display things)
  (newline))

;; Guile-specific

(use-modules (ice-9 pretty-print))
(define (evil e) (eval e (interaction-environment)))
