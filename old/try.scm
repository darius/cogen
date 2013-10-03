(try "test/2.scm" '((S D) (S D) (D)))
(power 5 'argument)

(try "test/kmp0.scm" '((S S) (S D) (S S S S D S) (S S D S)))
(search '(a b a b) 'argument)
(search '(a b c a b c a c a b) 'argument)

(try "self-app/self-eval.scm" 
     '((S S S D) (S S S D) (S S S D S) (S S S D S S) (S D) (S S S D S) (S S D) (S S S)))
(call '(fib) 
      '((define (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
      'fib
      'argument)

(call '(fact)
      '((define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))))
      'fact
      'argument)
