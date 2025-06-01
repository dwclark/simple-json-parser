(asdf:defsystem #:simple-json-parser
  :description "Simple Json Parser"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  ;:depends-on ("parse-float" "alexandria")
  :serial t
  :components ((:file "src/decode")
	       (:file "src/decode-event")
	       (:file "src/encode")))

(asdf:defsystem #:simple-json-parser-tests
  :description "Simple Json Parser Tests"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  :serial t

  :depends-on ("simple-json-parser" "fiveam" "alexandria" "uiop")

  :components ((:file "test/decodetests")
	       (:file "test/encodetests")))

