(defsystem "cafemielk-tests"
  :description "Test suite for Cafemielk"
  :version "0.0.1"
  :author "Nagamine, Hideaki <nagamine.hideaki.88x@kyoto-u.jp>"
  :license "BSD"
  :class :package-inferred-system
  :depends-on ("cafemielk"
               "cafemielk-tests/all"
               "fiveam")
  :perform (test-op (o c)
                    (symbol-call :fiveam :run! :cm-test-all)))
