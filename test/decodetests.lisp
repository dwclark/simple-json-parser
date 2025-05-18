(defpackage :simple-json-parser-tests
  (:use #:cl #:fiveam #:simple-json-parser))

(in-package :simple-json-parser-tests)

(def-suite sjp-tests)
(in-suite sjp-tests)

(test test-decode-constants
  (is (eq :true (decode "true")))
  (is (eq :false (decode "false")))
  (is (eq :null (decode "null"))))

(test test-decode-numbers
  (is (= 1 (decode "1")))
  (is (= 123 (decode "123")))
  (is (equalp 1e10 (decode "1e10")))
  (is (equalp 1.2e10 (decode "1.2e10")))
  (is (equalp 1.2e10 (decode "1.2e+10")))
  (is (= 1.257 (decode "1.257")))
  (is (= 1.245e-9 (decode "12.45e-10")))
  (is (= 1.783456e-10 (decode "178.3456e-12")))
  (is (= -1.783456e-10 (decode "-178.3456e-12")))
  (is (zerop (decode "0")))
  (is (zerop (decode "-0"))))

(test test-decode-string
  (is (string= "foo" (decode "\"foo\"")))
  (is (string= (decode "\"fo\\no\"") (make-array 4 :element-type 'character :initial-contents '(#\f #\o #\Linefeed #\o))))
  (is (string= (decode "\"t\\tab\"") (make-array 4 :element-type 'character :initial-contents '(#\t #\Tab #\a #\b))))
  (is (string= "foo" (decode "\"\\u0066\\u006f\\u006f\"")))
  (is (string= "foo" (decode "\"\\u0066\\u006F\\u006F\"")))
  )
