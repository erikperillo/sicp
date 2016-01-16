;exercise 21
(define (divides? a b)
		(= (remainder b a) 0))

(define (square x)
		(* x x))

(define (find-divisor n test-divisor)
		(cond ((> (square test-divisor) n) n)
			  ((divides? test-divisor n) test-divisor)
			  (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
		(find-divisor n 2))

;exercise 22
(define (prime? n)
		(= (smallest-divisor n) n))

(define (timed-prime-test n)
		(newline)
		(display n)
		(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
		(if (prime? n)
		(report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
		(display " *** ")
		(display elapsed-time))

;exercise 23
(define (next n)
		(if (= n 2) 
			3
			(+ n 2)))	

(define (fast-find-divisor n test-divisor)
		(cond ((> (square test-divisor) n) n)
			  ((divides? test-divisor n) test-divisor)
			  (else (fast-find-divisor n (next test-divisor)))))

(define (fast-smallest-divisor n)
		(fast-find-divisor n 2))

;exercise 28
(define (expmod base exp m)
		(cond ((= exp 0) 1)
			  ((= (square base) (remainder 1 n))
			   0)
		   	  ((even? exp)
			   (remainder (square (expmod base (/ exp 2) m))
						  m))
			  (else
			   (remainder (* base (expmod base (- exp 1) m))
				          m))))


(define (prime-iter n i)
		(cond ((= n i) 1)
			  ((= (expmod i n n) n)
			   (prime-iter n (+ i 1)))))
