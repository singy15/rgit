(defsystem "rgit"
  :version "0.1.0"
  :author "kedama"
  :license "MIT"
  :depends-on ("split-sequence" "md5")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "rgit/tests"))))

(defsystem "rgit/tests"
  :author ""
  :license ""
  :depends-on ("rgit"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for rgit"
  :perform (test-op (op c) (symbol-call :rove :run c)))
