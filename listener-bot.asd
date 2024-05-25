(defsystem "listener-bot"
  :version "0.0.1"
  :author "Giovanni Crisalfi"
  :license ""
  :depends-on ("clsql"
               "cl-telegram-bot"
               "clsql-sqlite3")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "listener-bot/tests")))
  :build-operation "program-op"
  :build-pathname "listener-bot"
  :entry-point "listener-bot:main")

(defsystem "listener-bot/tests"
  :author "Giovanni Crisalfi"
  :license ""
  :depends-on ("listener-bot"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for listener-bot"
  :perform (test-op (op c) (symbol-call :rove :run c)))
