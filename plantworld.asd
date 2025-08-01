(defsystem :plantworld
  :description "Plant simulation program based on Lindenmayer systems"
  :serial t
  :depends-on (:sdl2
               :alexandria
               :livesupport
               :cl-hash-util
               :noisy
               :cl-ppcre
               :str
               :fiveam
               :parse-number)
  :components ((:file "package")
               (:file "helpers")
               (:file "geometry")
               (:file "plantworld")
               (:file "parser"))
  :in-order-to ((test-op (test-op "plantworld/tests"))))


(defsystem :plantworld/tests
  :serial t
  :description "Test suite for Plantworld"
  :depends-on ("plantworld" "fiveam")
  :components
  ((:module #:test
    :components ((:file "test-parser"))))
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam :run!
                                      (uiop:find-symbol* :parser-tests :parser))))
