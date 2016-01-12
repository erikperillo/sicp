;exercise 1.7: make an method for the square root
(define (average x y)
		(/ (+ x y) 2.))

(define (better_guess guess x)
		(average (/ x guess) guess))

;assumes guess aways grows
(define (good_enough? guess last_guess)
		(<= (abs (- guess last_guess)) 
		   (*  guess 0.000001)))

(define (sqrt_iter guess last_guess x)
		(if (good_enough? guess last_guess)
			guess
			(sqrt_iter (better_guess guess x) guess x)))

(define (new_sqrt x)	
		(sqrt_iter 1. 0. x))

;exercise 1.8: make an method for the cube root
(define (square x)
		(* x x))

(define (better_guess_cube_root guess x)
		(/ (+ (/ x (square guess))
		      (* 2. guess))
		    3.))

(define (cube_root_iter guess last_guess x)
		(if (good_enough? guess last_guess)
			guess
			(cube_root_iter (better_guess_cube_root guess x) guess x)))

(define (cube_root x)
		(cube_root_iter 1. 2. x))
