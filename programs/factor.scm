(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (if (> (* test-divisor test-divisor) n) 
      n
      (if (divides? test-divisor n) 
	  test-divisor
	  (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))
