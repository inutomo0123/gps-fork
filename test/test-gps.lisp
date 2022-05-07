(load "../gpsaux.lisp")
(require :gps1 "../gps1.lisp")
(load "../gps.lisp")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(ql:quickload :1am)

(defpackage test/gps (:use :cl :1am :gps))

(in-package :test/gps)

;;; utilities

(test executing-p-test
  (is (eq (gps::executing-p '(gps::executing a b c))
          t)))


(test starts-with-test
  (is (eq (gps::starts-with '(a b c) 'a)
          t)))


(test member-equal-test
  (is (equal (gps::member-equal "b" '("a" "b" "c"))
             '("b" "c"))))


(test convert-op-test
  (is (every #'values
             (let ((op (gps::convert-op
                        (gps::make-op :action 'action
                                      :preconds '(preconds1 preconds2)
                                      :add-list '(add-list1 add-list2)
                                      :del-list '(del-list1 del-list2)))))
               (list (eq (gps::op-action op) 'action)
                     (equal (gps::op-preconds op) '(preconds1 preconds2))
                     (equal (gps::op-add-list op) '((gps::executing action)
                                                    add-list1
                                                    add-list2))
                     (equal (gps::op-del-list op) '(del-list1 del-list2)))))))


(test op-test
  (is (every #'values
             (let ((op (gps::op 'action
                                :preconds '(preconds1 preconds2)
                                :add-list '(add-list1 add-list2)
                                :del-list '(del-list1 del-list2))))
               (list (eq (gps::op-action op) 'action)
                     (equal (gps::op-preconds op) '(preconds1 preconds2))
                     (equal (gps::op-add-list op) '((gps::executing action)
                                                    add-list1
                                                    add-list2))
                     (equal (gps::op-del-list op) '(del-list1 del-list2)))))))


(test apply-op-test
  )


(test appropriate-p-test
  )
