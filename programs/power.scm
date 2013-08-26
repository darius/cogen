(define (power0 x n)
  (if (= n 0)
      1
      (* x (power0 x (- n 1)))))

(define (power x n)
  (if (= n 0)
      1
      (if (odd? n)
	  (* x (power x (- n 1)))
	  (square (power x (/ n 2))))))

(define (square y)
  (* y y))
