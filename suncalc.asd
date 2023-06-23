(in-package :asdf)

(defsystem "suncalc"
  :depends-on ("local-time")
  :description "Lisp implementation of Suncalc node module"
  :version "0.1"
  :author "Josh Armenta"
  :licence "BSD"
  :components ((:file "suncalc"))
)

(defsystem "suncalc/tests"
  :description "Unit tests for suncalc based on the Javascript implementation"
  :version "0.1"
  :author "Josh Armenta"
  :license "BSD")
