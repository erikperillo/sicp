(define tolerance 0.00001)

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
			(< (abs (- v1 v2)) tolerance))
	(define (try guess)
		    (display guess) (newline)
			(let ((next (f guess)))
				 (if (close-enough? guess next)
					 next
					 (try next))))
	(try first-guess))

(define (avg x0 x1)
		(/ (+ x0 x1) 2))

(define (sqrt x)
		(fixed-point (lambda (y) (avg y (/ x y)))
					 1.0))

;exercise 35
(define (fi)
		(fixed-point (lambda (y) (+ 1 (/ 1 y)))
					  1.0))

;exercise 36
(define (x_to_the_x)
		(fixed-point (lambda (y) (/ (log 1000) (log y)))
					 2.0))

;exercise 37
(define (cont-frac n d k)
		(define (find-frac i)
				(if (> i k)
					0
					(/ (n i) (+ (d i) (find-frac (+ i 1))))))
		(find-frac 1))


(define (fib k)
		(/ 1.0 (cont-frac (lambda (i) 1.0)
						  (lambda (i) 1.0)
						  k)))

(define (cont-frac-iter n d k)
		(define (find-frac-iter i result)
				(display result) (newline)
				(if (= i 0)
					result
					(find-frac-iter (- i 1)
									(/ (n i) 
									   (+ (d i) result)))))
		(find-frac-iter k 0))

(define (fib-iter k)
		(/ 1.0 (cont-frac-iter (lambda (i) 1.0)
							   (lambda (i) 1.0)
							   k)))

;exercise 38
(define (e-term k)
		(if (not (= (remainder (+ k 1) 3) 0))
			1.
			(* (/ (+ k 1) 3.) 2.)))

(define (print k)
		(define (loop i)
				(display (e-term i)) (newline)
				(if (= i k)
					0
					(loop (+ i 1))))
		(loop 1))

(define (e k)
		(+ 2 (cont-frac-iter (lambda (i) 1.0)
							 e-term
							 k)))

;exercise 39
(define (square x)
		(* x x))

(define (odd-term k)
		(- (* 2 k) 1.))

(define (tant x k)
		(define (exp-term k)
				(if (= k 1)
					x
					(* (square x) -1.)))
		(cont-frac-iter exp-term odd-term k))
