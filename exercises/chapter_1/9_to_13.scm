;exercise 11
;recursive proccess (tree iterative)
(define (f_rec n)
		(if (< n 3)
			n
			(+ (f (- n 1))
			   (* 2
				  (f (- n 2)))
			   (* 3
				  (f (- n 3))))))

;iterative process
(define (f_iter a b c count) 
		(if (= count 0) 
			c	
			(f_iter (+ a 
					   (* 2 b) 
					   (* 3 c))
					a
					b
					(- count 1))))
(define (f n)
		(f_iter 2 1 0 n))
			 
;exercise 12
(define (pascal row col)
		(if (or (= col 0) (= col row))
			1
			(+ (pascal (- row 1)
					   (- col 1))
			   (pascal (- row 1)
					   col))))

;prints pascal tree TODO: buggy :)
(define (print_tree_iter row col rows)
		(cond ((= row rows) (newline))
			  ((> col row) ((newline) (print_tree_iter (+ row 1) 0 rows)))
			  (else ((display (pascal row col)) 
					(print_tree_iter row (+ col 1) rows)))))

(define (print_tree rows)
		(print_tree_iter 0 0 rows))


