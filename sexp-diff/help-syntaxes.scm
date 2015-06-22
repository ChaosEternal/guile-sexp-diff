;;;; sexp-diff/help-syntaxes.scm --- help syntaxes for sexp-diff
;;;; Copied from guile's manual
;;;; Copyright (C) 1999, 2000, 2001, 2004, 2006, 2007, 2009, 2010,
;;;;   2011, 2012, 2013 Free Software Foundation, Inc.
;;;;
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this software; see the file COPYING.LESSER.
;;;; If not, write to the Free Software Foundation, Inc., 51 Franklin
;;;; Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (sexp-diff help-syntaxes)
  #:export (add1 sub1))

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
