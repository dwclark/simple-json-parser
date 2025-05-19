(in-package :simple-json-parser-tests)

(in-suite sjp-tests)

(test test-encode-constants ()
  (is (string= "null" (encode :null nil)))
  (is (string= "true" (encode :true nil)))
  (is (string= "false" (encode :false nil))))
  
(test test-encode-numbers ()
  (is (string= "0" (encode 0 nil)))
  (is (string= "100" (encode 100 nil)))
  (is (string= "1.23e+0" (encode 1.23d0 nil)))
  (is (string= "1.23e+0" (encode 1.23e0 nil))))

(test test-encode-strings ()
  (loop for str in '("asdf" ";.</:" "~!!@$%^&*(097" "")
	do (is (string= (concatenate 'string "\"" str "\"") (encode str nil)))))

(test test-encode-arrays ()
  (is (string= "[]" (encode (vector) nil)))
  (is (string= "[1,\"123\",null]" (encode (vector 1 "123" :null) nil)))
  (is (string= "[1,\"123\",[4,5,6]]" (encode (vector 1 "123" (vector 4 5 6)) nil))))

(test test-encode-hash-table ()
  (is (string= "{}" (encode (make-hash-table) nil)))
  (is (string= "{\"one\":1,\"two\":2}" (encode (alexandria:alist-hash-table '(("one" . 1) ("two" . 2))) nil)))
  (is (string= "{\"one\":1,\"two\":2,\"three\":[1,2,3]}"
	       (encode (alexandria:alist-hash-table '(("one" . 1) ("two" . 2) ("three" . #(1 2 3)))) nil))))

(test test-roundtrip
  (let ((one "{\"one\":1,\"two\":2,\"three\":[1,2,3]}")
	(two "{\"one\":1,\"two\":2,\"three\":[1,2,3,null,true,false]}"))
    (is (string= one (encode (decode one) nil)))
    (is (string= two (encode (decode two) nil)))))
