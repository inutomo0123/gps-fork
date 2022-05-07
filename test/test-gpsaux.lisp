(load "../gpsaux.lisp")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(ql:quickload :1am)

(defpackage test/gpsaux (:use :cl :gps :1am))

(in-package :test/gpsaux)


;;; auxiliary

(test find-all-if-test
  (is (equal (gps::find-all-if #'numberp '(a 1 b 2 c 3))
             '(1 2 3)))
  (is (equal (gps::find-all-if #'numberp '(a 1 (b 2 (c 3))))
             '(1))))


(test find-all-test
  ;; symbol
  (is (equal (gps::find-all 'a '(a b c a b c))
             '(a a)))
  (is (equal (gps::find-all 'a '(a b c (a b c)))
             '(a)))
  ;; atom
  (is (equal (gps::find-all 2 '(1 2 3 1 2 3))
             '(2 2)))
  (is (equal (gps::find-all 3 '(1 2 3 1 2 3) :test #'=)
             '(3 3)))
  (is (equal (gps::find-all 3 '(1 2 3 1 2 3) :test-not #'=)
             '(1 2 1 2)))
  (is (equal (gps::find-all 1 '(1 2 3 1 2 3) :start 0 :end 3)
             '(1 1 2 3))))


(test mappend-test
  (is (equal (gps::mappend (lambda (x) (list (* 2 x))) '(1 2 3))
             '(2 4 6))))


(test mklist-test
  (is (equal (gps::mklist 'a)
             '(a)))
  (is (equal (gps::mklist '(a))
             '(a))))


(test flatten-test
  (is (equal (gps::flatten '(a b c (a b c)))
             '(a b c a b c)))
  (is (equal (gps::flatten '(a b (c d (e f))))
             '(a b c d (e f)))))


(test random-elt-test
  (is (let ((list '(1 2 3 4 5 6)))
        (find (gps::random-elt list) list)))
  (is (let ((list `(,(gensym) ,(gensym) ,(gensym))))
        (find (gps::random-elt list) list))))

;;; debug

(test dbg-test
  (is (string=
       (let ((gps::*dbg-ids* nil))
         (declare (special gps::*dbg-ids*))
         (setq gps::*dbg-ids* (cons :id1 gps::*dbg-ids*))
         (with-output-to-string (*debug-io*)
           (gps::dbg :id1 "~a ~a ~a" 'arg1 'arg2 'arg3)))
       "ARG1 ARG2 ARG3")))


(test undebug-test
  (is (equal (let ((gps::*dbg-ids* '(:id1 :id2 :id3)))
               (declare (special gps::*dbg-ids*))
               (gps::undebug :id3 :id1))
             '(:id2)))
  (is (eq (let ((gps::*dbg-ids* '(:id1 :id2 :id3)))
            (declare (special gps::*dbg-ids*))
            (gps::undebug))
          nil)))


(test dbg-indent-test
  (is (string= (let ((gps::*dbg-ids* '(:id1 :id2 :id3)))
                 (declare (special gps::*dbg-ids*))
                 (with-output-to-string (*debug-io*)
                   (gps::dbg-indent :id3 2 "~a" :id3)
                   (gps::dbg-indent :id1 0 "~a" :id1)))
               "    ID3
ID1")
      ))
