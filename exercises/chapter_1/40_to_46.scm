(define tolerance 0.001)

(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) (* tolerance (abs v2))))

(define (fixed-point f first-guess)
		(define (try guess)
				(let ((next (f guess)))
					 (if (close-enough? guess next)
						 next
						 (try next))))
		(try first-guess))

(define (average a b)
		(/ (+ a b) 2.0))

(define (average-damp f)
		(lambda (x) (average x (f x))))

(define (sqrt-new x)
		(fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

;exercise 40
(define (cube x)
		(* x x x))

(define (square x)
		(* x x))

(define dx 0.000001)

(define (deriv f)
		(lambda (x) 
			    (/ (- (f (+ x dx)) 
					  (f x)) 
				   dx))) 

(define (newton-transform f)
		(lambda (x) (- x 
					   (/ (f x) 
						  ((deriv f) x)))))

(define (newton-method f guess)
		(fixed-point (newton-transform f) guess))

(define (cubic a b c)
		(lambda (x) (+ (cube x)
					   (* a (square x)) 
					   (* b x)
					   c)))

;exercise 41
(define (inc x)
		(+ x 1))

(define (double f)
		(lambda (x) (f (f x))))


;exercise 42
(define (compose f g)
		(lambda (x) (f (g x))))


;exercise 43
(define (itentity x)
		x)

(define (repeated f n)
		(if (= n 0)
			identity
			(compose f (repeated f (- n 1)))))

(define (repeated-iter f n)
		(define (loop k result)
				(if (= k n)
					result
					(loop (+ k 1) (compose f result))))
		(loop 1 f))

;exercise 44
(define (smooth f)
		(lambda (x) (/ (+ (f (- x dx)) 
						  (f x) 
					      (f (+ x dx))) 
				    3.0)))

(define (n-fold-smooth f n)
		((repeated smooth n) f))

;exercise 46
(define (iterative-improve good-enough? improve)
		(lambda (x)
				(let ((next (improve x)))
					 (if (good-enough? x next)
						 next
						 ((iterative-improve good-enough? improve) next)))))

(define (crazy-sqrt x)
		((iterative-improve close-enough? 
						   (lambda (y) (/ (+ y (/ x y)) 2.0)))
		1.0))
						   
