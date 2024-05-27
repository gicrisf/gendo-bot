(defpackage listener-bot
  (:use :cl)
  (:import-from #:cl-telegram-bot
                #:start-processing
                #:on-message
                #:defbot)
  (:import-from #:cl-telegram-bot/chat
                #:private-chat
                #:get-username)
  (:import-from #:cl-telegram-bot/message
                #:reply
                #:get-current-chat)
  (:export #:main))

(in-package :listener-bot)

(defun main ()
  ;; connect to the db
  (let* ((path (or (uiop:getenv "DB_PATH")
                   ;; hardcoded default
                   "~/.bots/listener.db"
                   ;; (error "Define DB_PATH env var.")
                   ))
         ;; expanded path
         (path (uiop:native-namestring path)))
    (clsql:connect `(,path) :database-type :sqlite3))

  ;; check the tables
  (unless (clsql:table-exists-p 'messages)
    (let ((attributes
            '(message_id text date reply_to_message_id chat_id from_id))
          (values
            '(message-id text date reply-to-message-id chat-id from-id)))
      (clsql:create-table 'messages
                          '(([message_id] integer)
                            ([text] text)
                            ([date] integer)
                            ([reply_to_message_id] integer)
                            ([chat_id] integer)
                            ([from_id] integer)))
      (log:info "database created.")
      ;; and insert the first data
      (clsql:insert-records
       :into 'messages
       :attributes attributes
       :values values)))

  ;; TODO use make-instance and drop the macro
  (defbot listener-bot)

  (defmethod on-message
      ((bot listener-bot)
       text)
    ;; collect data from the last message
    (let* ((raw-data (cl-telegram-bot/message:get-raw-data
                      cl-telegram-bot/message::*current-message*))
           (message-id (getf raw-data ':|message_id|))
           (text (getf raw-data ':|text|))
           (date (getf raw-data ':|date|))
           (reply-to-message (getf raw-data ':|reply_to_message|))
           (reply-to-message-id (getf reply-to-message ':|message_id|))
           (chat (getf raw-data ':|chat|))
           (chat-id (getf chat ':|id|))
           (from (getf raw-data ':|from|))
           (from-id (getf from ':|id|)))
      (log:info "Store message data into DB.")
      (let ((attributes
              '(message_id text date reply_to_message_id chat_id from_id))
            (values
              `(,message-id ,text ,date ,reply-to-message-id ,chat-id ,from-id)))
        (if (clsql:table-exists-p 'messages)
            ;; the table exists already, write in it.
            (clsql:insert-records
             :into 'messages
             :attributes attributes
             :values values)))))

  (defmethod on-message
      ((bot listener-bot)
       text)
    (let* ((chat (cl-telegram-bot/message:get-current-chat))
           (username (cl-telegram-bot/chat:get-username chat))
           (raw-data (cl-telegram-bot/message:get-raw-data
                      cl-telegram-bot/message::*current-message*)))
      (reply "poccu sanapune")))

  (defmethod on-command
      ((bot listener-bot)
       (command (eql :help))
       text)
    (declare (ignorable text))
    (reply "Just send me any text and I'll reply with the same text."))

  (defmethod on-command
      ((bot listener-bot)
       (command (eql :start))
       text)
    (declare (ignorable text))
    (reply "Welcome Lisper! Have a fun, playing with cl-telegram-bot!"))

  (start-processing
   (make-instance
    'listener-bot
    :token (or (uiop:getenv "TELEGRAM_TOKEN")
               (error "Define TELEGRAM_TOKEN env var.")))
   :debug t)

  ;; keep the process open
  (read-line))
