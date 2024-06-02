(defsystem "gendo-bot"
  :version "0.0.1"
  :author "Giovanni Crisalfi"
  :license ""
  :depends-on ("cl-telegram-bot"
               "clsql"
               "clsql-sqlite3")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "gendo-bot/tests")))
  :build-operation "program-op"
  :build-pathname "gendobot"
  :entry-point "gendobot:main")

(defsystem "gendo-bot/tests"
  :author "Giovanni Crisalfi"
  :license ""
  :depends-on ("gendo-bot"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for gendo-bot"
  :perform (test-op (op c) (symbol-call :rove :run c)))
