(load "../gpsaux.lisp")
(require :gps1 "../gps1.lisp")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(ql:quickload :1am)

(defpackage test/gps1 (:use :cl :gps :1am))

(in-package :test/gps1)

(test op-test
  (is (equal (let ((op (gps::make-op :action 'action
                                     :preconds '(preconds)
                                     :add-list '(add-list)
                                     :del-list '(del-list))))
               (list (gps::op-action op)
                     (gps::op-preconds op)
                     (gps::op-add-list op)
                     (gps::op-del-list op)))
             '(action (preconds) (add-list) (del-list)))))


(test appropriate-p1-test
  (let ((op (gps::make-op :action 'action
                          :preconds '(preconds)
                          :add-list '(add-list1 add-list2)
                          :del-list '(del-list1 del-list2))))
    (is (equal (gps::appropriate-p1 'add-list1 op)
               '(add-list1 add-list2)))
    (is (equal (gps::appropriate-p1 'add-list2 op)
               '(add-list2)))))


(test apply-op1-test
  (let ((op (gps::make-op :action 'drive-son-to-school
                          :preconds '(son-at-home car-works)
                          :add-list '(son-at-school)
                          :del-list '(son-at-home))))
    ;; 戻り値
    (is (eq (let ((gps::*state* '(son-at-home car-works)))
              (declare (special gps::*state*))
              (gps::apply-op1 op))
            t))
    (is (eq (let ((gps::*state* '(son-at-home)))
              (declare (special gps::*state*))
              (gps::apply-op1 op))
            nil))

    ;; 標準出力 副作用
    (is (string= (with-output-to-string (*standard-output*)
                   (let ((gps::*state* '(son-at-home car-works)))
                     (declare (special gps::*state*))
                     (gps::apply-op1 op)))
                 "
(GPS::EXECUTING TEST/GPS1::DRIVE-SON-TO-SCHOOL) "))

    (is (string= (with-output-to-string (*standard-output*)
                   (let ((gps::*state* '(son-at-home)))
                     (declare (special gps::*state*))
                     (gps::apply-op1 op)))
                 ""))

    ;; 大域変数 *state* 副作用
    (is (equal (let ((gps::*state* '(son-at-home car-works)))
                 (declare (special gps::*state*))
                 (gps::apply-op1 op)
                 gps::*state*)
               '(CAR-WORKS SON-AT-SCHOOL)))
    (is (equal (let ((gps::*state* '(son-at-home)))
                 (declare (special gps::*state*))
                 (gps::apply-op1 op)
                 gps::*state*)
               '(SON-AT-HOME)))))

(test achieve1-test
  ;; *state*が既にゴールしている
  (is (equal (let ((gps::*state* '(son-at-home car-works)))
               (gps::achieve1 'son-at-home))
             '(SON-AT-HOME CAR-WORKS)))
  ;; *state*がゴールする条件を満たしている
  (is (eq (let ((gps::*ops*
                  (list
                   (gps::make-op :action 'drive-son-to-school
                                 :preconds '(son-at-home car-works)
                                 :add-list '(son-at-school)
                                 :del-list '(son-at-home))
                   (gps::make-op :action 'shop-installs-battery
                                 :preconds '(car-needs-battery
                                             shop-knows-problem
                                             shop-has-money)
                                 :add-list '(car-works))
                   (gps::make-op :action 'tell-shop-problem
                                 :preconds '(in-communication-with-shop)
                                 :add-list '(shop-knows-problem))
                   (gps::make-op :action 'telephone-shop
                                 :preconds '(know-phone-number)
                                 :add-list '(in-communication-with-shop))
                   (gps::make-op :action 'look-up-number
                                 :preconds '(have-phone-book)
                                 :add-list '(know-phone-number))
                   (gps::make-op :action 'give-shop-money ; これ
                                 :preconds '(have-money)
                                 :add-list '(shop-has-money) ; ゴール
                                 :del-list '(have-money))))
                (gps::*state* '(have-money)))
            (declare (special gps::*ops* gps::*state*))
            (gps::achieve1 'shop-has-money))
          t)))


(test GPS1-test
  (is (eq (let ((gps::*ops*
                  (list
                   (gps::make-op :action 'drive-son-to-school
                                 :preconds '(son-at-home ; これ
                                             car-works)
                                 :add-list '(son-at-school) ; ゴール
                                 :del-list '(son-at-home))
                   (gps::make-op :action 'shop-installs-battery
                                 :preconds '(car-needs-battery ; これ
                                             shop-knows-problem
                                             shop-has-money)
                                 :add-list '(car-works))
                   (gps::make-op :action 'tell-shop-problem
                                 :preconds '(in-communication-with-shop) ; これ
                                 :add-list '(shop-knows-problem))
                   (gps::make-op :action 'telephone-shop
                                 :preconds '(know-phone-number) ; これ
                                 :add-list '(in-communication-with-shop))
                   (gps::make-op :action 'look-up-number
                                 :preconds '(have-phone-book) ; これ
                                 :add-list '(know-phone-number))
                   (gps::make-op :action 'give-shop-money
                                 :preconds '(have-money) ; これ
                                 :add-list '(shop-has-money)
                                 :del-list '(have-money))))
                (gps::*state* '(have-money
                                have-phone-book
                                know-phone-number
                                in-communication-with-shop
                                car-needs-battery
                                son-at-home)))
            (declare (special gps::*ops* gps::*state*))
            (gps::GPS1 gps::*state* '(son-at-school) gps::*ops*))
          'GPS::SOLVED)))
