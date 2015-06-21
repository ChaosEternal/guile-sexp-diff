
(define-module (tests sexp-diff-test-suite)
  #:use-module (test-suite lib)
  #:use-module (sexp-diff diff))

(with-test-prefix
  "sexp-diff/diff.scm"
  (pass-if "sexp-diff: case 1" 
	   (equal? 
	    '((DEFUN F (X) (#:new - #:old + (* X 2) #:new 3 1)))
	    (sexp-diff
	     '(DEFUN F (X) (+ (* X 2) 1))
	     '(DEFUN F (X) (- (* X 2) 3 1)))))
  (pass-if "sexp-diff: case 2"
	  (equal?
	   '((DEFUN F (X) (#:new - #:old + (* X 2) #:new 5 #:new 3 #:old 4 1)))
	   (sexp-diff
	    '(DEFUN F (X) (+ (* X 2) 4 1))
	    '(DEFUN F (X) (- (* X 2) 5 3 1)))))
  (pass-if "sexp-diff: case 3"
	   (equal?
	    '((DEFUN F (X)
		(#:new - #:old + (* X 2) #:new 5 #:new 5 #:new 3 #:old 4 #:old 4 1)))
	    (sexp-diff
	     '(DEFUN F (X) (+ (* X 2) 4 4 1))
	        '(DEFUN F (X) (- (* X 2) 5 5 3 1))))))

	   
