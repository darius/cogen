;; First, some nonportable functions (at least nonportable from
;; my ancient R4RS Scheme):

;(define error @error)

; Evaluate EXPR in the top-level interaction environment.
(define (eval-top expr)
  (eval expr))


(load "misc.scm")
(load "stuff.scm")
;(load "cogen.scm")
;(load "identity.scm")

(load "common.scm")
(load "self-app/primitives.scm")
(load "mono-cogen.scm")


(define (try filename f div residuals)
  (cogen (snarf filename) f div residuals))


(define (try-mono-cogen filename divs)
  (let* ((defs (snarf filename))
	 (program (make-program defs divs)))
    (for-each emit-code
	      (stage-em defs program))
    program))
