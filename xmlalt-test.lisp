(in-package :xmlalt-tests)

(5am:def-suite :suite1 :description "Xmlalt Test Suite")
(5am:in-suite :suite1)

(5am:test
 pop-util-equal--1
 (5am:is (equal '((1) (2))
                '((1) (2)))))


