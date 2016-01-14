;exercise 16
(define (square x) (* x x))

(define (fast_exp_iter b n a)
		(cond ((<= n 0) a) 
			  ((= (remainder n 2) 0) (fast_exp_iter (square b) (/ n 2) a))
			  (else (fast_exp_iter b (- n 1) (* a b)))))

(define (fast_exp b n)
		(fast_exp_iter b n 1))

;exercise 17
(define (double x)
		(* x 2))

(define (halve x)
		(/ x 2))

(define (fast_mult_iter b n a)
		(cond ((<= n 0) a)
			  ((= (remainder n 2) 0) (fast_mult_iter (double b) (halve n) a))
			  (else (fast_mult_iter b (- n 1) (+ a b)))))

(define (fast_mult b n)
		(fast_mult_iter b n 0))

;exercise 19
(define (fib n)
		(fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
		(cond ((= count 0) b)
			  ((even? count)
			   (fib-iter a
						 b
						 (+ (square p) (square q)) ;compute p’
						 (+ (double (* p q)) (square q)) ;compute q’
						 (/ count 2)))
			  (else (fib-iter (+ (* b q) (* a q) (* a p))
							  (+ (* b p) (* a q))
							  p
           				      q
							  (- count 1)))))

