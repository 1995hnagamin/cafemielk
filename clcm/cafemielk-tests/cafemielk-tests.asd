(defsystem "cafemielk-tests"
  :description "Test suite for Cafemielk"
  :version "0.0.1"
  :author "Nagamine, Hideaki <nagamine.hideaki.88x@kyoto-u.jp>"
  :license "BSD"
  :class :package-inferred-system
  :depends-on ("cafemielk"
               "cafemielk-tests/all")
  :perform (test-op (o c)
                    (funcall (find-symbol* :run-tests :cafemielk-tests/all))))
