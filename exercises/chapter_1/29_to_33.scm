(define (sum term a next b)
		(if (> a b)
			  0
			(+ (term a)
			   (sum term (next a) next b))))

(define (square x)
		(* x x))

(define (succ x)
		(+ x 1))

(define (identity x) 
		x)

;exercise 29
(define (cube x)
		(* x x x))

(define (even x)
		(= (remainder x 2) 0))

(define (simpson f a b n)
		(define h (/ (- b a) n))

		(define (yk k)
				(f (+ a (* k h))))

		(define (term k)
				(cond ((or (= k 0) (= k n)) (yk k))
					  ((even k) (* (yk k) 2))
					  (else (* (yk k) 4))))

		(* (/ h 3.0) (sum term 0 succ n)))

;exercise 31
(define (product term a next b)
		(if (> a b)
			  1
			(* (term a)
			   (product term (next a) next b))))

(define (prod_iter term a next b)
		(define (iter k result)
				(if (> k b)
					result
				   	(iter (next k) (* result (term k)))))
		(iter a 1))

(define (factorial n)
		(prod_iter identity 1 succ n))

(define (succ_of_succ x)
		(succ (succ x)))

(define (last_even k)
		(- k (remainder k 2)))

;what a slow convergence!
(define (pi n)
		(* 4.0
		   (/ (* 2 
			     (square (prod_iter identity 4 succ_of_succ (- (last_even n) 2))) 
			     (last_even n))
			  (square (prod_iter identity 3 succ_of_succ (- (last_even n) 1))))))

;exercise 32
(define (accumulate combiner null_val term a next b)
		(if (> a b)
			null_val
			(combiner (term a) 
					  (accumulate combiner null_val term (next a) next b))))

(define (accumulate_iter combiner null_val term a next b)
		(define (iter k result)
				(if (> k b)
					result
					(iter (next k) (combiner (term k) result))))
		(iter a null_val))

(define (new_sum term a next b)
		(accumulate + 0 term a next b))

(define (new_prod term a next b)
		(accumulate * 1 term a next b))

(define (new_prod_iter term a next b)
		(accumulate_iter * 1 term a next b))

(define (new_fact_iter n)
		(new_prod_iter identity 1 succ n))


;exercise 33
(define (divides? a b)
		(= (remainder b a) 0))

(define (find-divisor n test-divisor)
		(cond ((> (square test-divisor) n) n)
			  ((divides? test-divisor n) test-divisor)
			  (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
		(find-divisor n 2))

(define (prime? n)
		(and (= (smallest-divisor n) n) (> n 1)))

(define (filter_accum comb null_val term a next b filter)
		(if (> a b)
			null_val
			(comb (if (filter a) (term a) null_val)
				  (filter_accum comb null_val term (next a) next b filter))))

(define (sum_prime_squares a b)
		(filter_accum + 0 square a succ b prime?))
