(define (append prefix suffix)
  (if (null? prefix)
      suffix
      (cons (car prefix)
            (append (cdr prefix) suffix))))
