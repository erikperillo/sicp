(define (pn number) (display number))
(define (ps str) (write str))
(define nl newline)

;exercise 1.1: print the result of these things

(write "ex 1:") (newline)
(display 10) (newline)
(display (+ 5 3 4)) (newline)
(display (- 9 1)) (newline)
(display (/ 6 2)) (newline)
(display (+ (* 2 4) (- 4 6))) (newline)
(define a 3)
(define b (+ a 1))
(display (+ a b (* a b))) (newline)
(display (= a b)) (newline)
(display (if (and (> b a) (< b (* a b)))
b
a)) (newline)
(display (cond ((= a 4) 6)
((= b 4) (+ 6 7 a))
(else 25))) (newline)
(display (+ 2 (if (> b a) b a))) (newline)
(display (* (cond ((> a b) a)
((< a b) b)
(else -1))
(+ a 1))) (newline)


;exercise 1.2: write <complex fraction> in prefix notation

(newline) (write "ex 2:") (newline)
(define result
		(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))                                     
		   (* 3 (- 6 2) (- 2 7))))
(display result) (newline)


;exercise 1.3: define a procedure that takes three numbers as arguments 
;and returns the sum of the squares of the two larger numbers.

(define (square x) 
		(* x x))
(define (sum_squares x y z) 
		(+ (square x) (square y) (square z)))

;test
(newline) (write "ex 3:") (newline)
(write "(sum_squares 1 2 3) = ") (display (sum_squares 1 2 3)) (newline)
