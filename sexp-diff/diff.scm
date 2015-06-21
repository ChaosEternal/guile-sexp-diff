;;;; sexp-diff/diff.scm - stolen from Chicken/sexp-diff.scm
;;;; sexp-diff.scm - stolen from Racket/unstable


;;; diff-sexp.lisp -- diffs s-expressions based on Levenshtein-like edit distance.

;; Author:	   Michael Weber <michaelw@foldr.org>
;; Date:	   2005-09-03
;; Modified:	   2005-09-04
;; Modified:	   2005-09-07
;; Modified:	   2005-09-15
;; Modified:    2010-06-22 (Ported to racket by Vincent St-Amour)
;; Modified:       2015-06-21 (Ported to guile by chaoseternal@gmail.com)
;;
;; This code is in the Public Domain.

;;; Description:

;; DIFF-SEXP computes a diff between two s-expressions which minimizes
;; the number of atoms in the result tree, also counting edit
;; conditionals #:new, #:old.

;;; Examples:

;; > (sexp-diff
;;    '(DEFUN F (X) (+ (* X 2) 1))
;;    '(DEFUN F (X) (- (* X 2) 3 1)))
;; ((DEFUN F (X) (#:new - #:old + (* X 2) #:new 3 1)))
;; > (sexp-diff
;;    '(DEFUN F (X) (+ (* X 2) 4 1))
;;    '(DEFUN F (X) (- (* X 2) 5 3 1)))
;; ((DEFUN F (X) (#:new - #:old + (* X 2) #:new 5 #:new 3 #:old 4 1)))
;; > (sexp-diff
;;    '(DEFUN F (X) (+ (* X 2) 4 4 1))
;;    '(DEFUN F (X) (- (* X 2) 5 5 3 1)))
;; ((DEFUN F (X) #:new (- (* X 2) 5 5 3 1) #:old (+ (* X 2) 4 4 1)))

;;; Todo:

;; * Support for moved subtrees
;; * The algorithm treats vectors, arrays, etc. as opaque objects
;; * This article might describe a better method (unchecked):
;;   Hélène Touzet: "A linear tree edit distance algorithm for similar ordered trees"
;;   LIFL - UMR CNRS 8022 - Université Lille 1
;;   59 655 Villeneuve d'Ascq cedex, France
;;   Helene.Touzet@lifl.fr


(define-module (sexp-diff diff)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (sexp-diff))


(define-syntax add1
  (lambda (x)
    (syntax-case x ()
      ((_ exp)
       (syntax (+ exp 1))))))

(define-syntax sub1
  (lambda (x)
    (syntax-case x ()
      ((_ exp)
       (syntax (- exp 1))))))

;; Computes the number of atoms contained in TREE.
(define (tree-size tree)
  (if (pair? tree)
      (apply + 1 (map tree-size tree))
      1))

;; edit-record
;;  type: 'unchanged | 'deletion | 'insertion | 'update | 'compound
;;  edit-distance
;;  change: CHANGE | (OLD . NEW) | (CHANGE ...)
(define-record-type 
  edit-record
  (make-edit-record type edit-distance change)
  edit-record?
  (type edit-record-type)
  (edit-distance edit-record-edit-distance)
  (change edit-record-change))

(define (make-unchanged-record change)
  (make-edit-record 'unchanged (tree-size change) change))

(define (make-deletion-record change)
  (make-edit-record 'deletion (add1 (tree-size change)) change))

(define (make-insertion-record change)
  (make-edit-record 'insertion (add1 (tree-size change)) change))

(define (make-update-record old new)
  (make-edit-record 'update
		    (+ 1 (tree-size old)
		       1 (tree-size new))
		    (cons old new)))

(define (make-compound-record changes)
  (make-edit-record
   'compound
   (apply + (map edit-record-edit-distance changes))
   changes))

(define (make-empty-compound-record)
  (make-compound-record '()))

(define (make-extend-compound-record r0 record)
  (make-compound-record (cons record (get-change r0))))

(define (unchanged-record? r) (eq? (edit-record-type r) 'unchanged))
(define (deletion-record? r) (eq? (edit-record-type r) 'deletion))
(define (insertion-record? r) (eq? (edit-record-type r) 'insertion))
(define (compound-record? r) (eq? (edit-record-type r) 'compound))
(define (update-record? r) (eq? (edit-record-type r) 'update))

(define (get-change record) (edit-record-change record))

(define (render-difference record)
  (cond [(insertion-record? record)
	 (list '#:new (edit-record-change record))]
	[(deletion-record? record)
	 (list '#:old (edit-record-change record))]
	[(update-record? record)
	 (let ((change (edit-record-change record)))
	   (list '#:old (car change)
		 '#:new (cdr change)))]
	[(unchanged-record? record)
	 (list (edit-record-change record))]
	[(compound-record? record)
	 (list (fold (lambda (r res)
		       (append res (render-difference r)))
		     '()
		     (reverse (edit-record-change record))))]))

;; Returns record with minimum edit distance.
(define (min/edit record . records)
  (fold-right (lambda (a b)
		(if (<= (edit-record-edit-distance a)
			(edit-record-edit-distance b))
		    a
		    b))
	      record
	      records))

;; Prepares initial data vectors for Levenshtein algorithm from LIST.
(define (initial-distance function lst)
  (let ((seq (make-vector (add1 (length lst)) (make-empty-compound-record))))
    (do ((i 0 (add1 i))
	 (elt lst (cdr elt)))
	((null? elt))
      (vector-set! seq (add1 i)
		   (make-extend-compound-record (vector-ref seq i)
						(function (car elt)))))
    seq))

;; Calculates the minimal edits needed to transform OLD-TREE into NEW-TREE.
;; It minimizes the number of atoms in the result tree, also counting
;; edit conditionals.
(define (levenshtein-tree-edit old-tree new-tree)
  (cond
   ((equal? old-tree new-tree)
    (make-unchanged-record old-tree))
   ((not (and (pair? old-tree) (pair? new-tree)))
    (make-update-record old-tree new-tree))
   (else
    (min/edit
     (make-update-record old-tree new-tree)
     (let* ((best-edit #f)
	    (row (initial-distance make-deletion-record old-tree))
	    (col (initial-distance make-insertion-record new-tree)))
       (do ((new-part new-tree (cdr new-part))
	    (currentlist (cdr (vector->list col)) (cdr currentlist)))
	   ((or (null? new-part) (null? currentlist)))
	 (let ((current (car currentlist)))
	   (do ((old-part old-tree (cdr old-part))
		(row-idx 0 (add1 row-idx)))
	       ((null? old-part))
	     (set! best-edit
		   (min/edit
		    (make-extend-compound-record
		     (vector-ref row (add1 row-idx))
		     (make-insertion-record (car new-part)))
		    (make-extend-compound-record
		     current
		     (make-deletion-record (car old-part)))
		    (make-extend-compound-record
		     (vector-ref row row-idx)
		     (levenshtein-tree-edit (car old-part) (car new-part)))))
	     (vector-set! row row-idx current)
	     (set! current best-edit)))
	 (vector-set! row (sub1 (vector-length row)) best-edit))
       best-edit)))))

;; Computes a diff between OLD-TREE and NEW-TREE which minimizes the
;; number of atoms in the result tree, also counting inserted edit conditionals
;; #:new, #:old.
(define (sexp-diff old-tree new-tree)
  (render-difference (levenshtein-tree-edit old-tree new-tree)))


