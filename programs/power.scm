(define (power0 n x)
  (if (= n 0)
      1
      (* x (power0 (- n 1) x))))

(define (power n x)
  (if (= n 0)
      1
      (if (odd? n)
	  (* x (power (- n 1) x))
	  (square (power (/ n 2) x)))))

(define (square y)
  (* y y))
