(defpackage gendo-bot
  (:use :cl)
  (:import-from #:cl-telegram-bot
                #:start-processing
                #:on-message
                #:on-command
                #:defbot)
  (:import-from #:cl-telegram-bot/chat
                #:private-chat
                #:get-username)
  (:import-from #:cl-telegram-bot/message
                #:reply
                #:get-current-chat)
  (:export #:main))

(in-package :gendo-bot)

(defun create-messages-table ()
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
    (log:info "MESSAGES table created.")))

(defun create-users-table ()
  (let ((attributes
          '(language_code username first_name is_bot from_id))
        (values
          '(language-code username first-name is-bot from-id)))
    (clsql:create-table 'users
                        '(([from_id] integer)
                          ([language_code] text)
                          ([username] text)
                          ([first_name] text)
                          ([is_bot] boolean)))
    (log:info "USERS table created.")))

(let ((db-path (quote "~/.bots/gendo.db")))
(let* ((path (if db-path (format nil "~A" db-path)
                 (uiop:getenv "GENDO_DB_PATH")))
       (path (if path path "~/.bots/default.db"))
       (path (uiop:native-namestring path))
       (db-exists (probe-file path)))
  (unless db-exists
    (clsql:create-database `(,path) :database-type :sqlite3))
  (clsql:connect `(,path) :database-type :sqlite3)))

;; check the tables
(unless (clsql:table-exists-p 'messages)
  (create-messages-table))

(unless (clsql:table-exists-p 'users)
  (create-users-table))

(cl-telegram-bot:defbot gendo-bot)

(setf *gendo-bot*
      ;; Dear Unknown,
      ;; you have to understand that I'm not crazy (not this crazy)
      ;; the point of this passage is that
      ;; `token` could be defined via org-mode facilities
      (let* ((token (if token token
                        (uiop:getenv "GENDO_TOKEN")))
             (token (if token token
                        (error "Define GENDO_TOKEN env var."))))
        (make-gendo-bot token)))

(defmethod cl-telegram-bot:on-message
    ((bot gendo-bot) text)
  ;; collect data from the last message
  (let* ((raw-data (cl-telegram-bot/message:get-raw-data
                    cl-telegram-bot/message::*current-message*))
         (message-id (getf raw-data ':|message_id|))
         (text (getf raw-data ':|text|))
         (text (string-trim "\"" text))
         (text (format nil "\"~A\"" text))
         (date (getf raw-data ':|date|))
         (reply-to-message (getf raw-data ':|reply_to_message|))
         (reply-to-message-id (getf reply-to-message ':|message_id|))
         (chat (getf raw-data ':|chat|))
         (chat-id (getf chat ':|id|))
         (from (getf raw-data ':|from|))
         (from-id (getf from ':|id|)))
    (log:info "Store message data into DB.")
    ;; I should probably write a macro to do this
    (let ((attributes '(message_id text date reply_to_message_id chat_id from_id))
          (values `(,message-id ,text ,date ,reply-to-message-id ,chat-id ,from-id)))
      (clsql:insert-records
       :into 'messages
       :attributes attributes
       :values values))))

(defmethod cl-telegram-bot:on-command
    ((bot gendo-bot)
     (command (eql :help)) text)
  (declare (ignorable text))
  (cl-telegram-bot:reply "Just send me any text and I'll reply with the same text."))

(defmethod cl-telegram-bot:on-command
    ((bot gendo-bot)
     (command (eql :start)) text)
  (declare (ignorable text))
  (cl-telegram-bot:reply "Welcome Lisper! Have a fun, playing with cl-telegram-bot!"))

(defmethod cl-telegram-bot:on-command
    ((bot gendo-bot)
     (command (eql :register)) text)
  (let* ((raw-data (cl-telegram-bot/message:get-raw-data
                    cl-telegram-bot/message::*current-message*))
         (date (getf raw-data ':|date|))
         (from (getf raw-data ':|from|))
         (from-id (getf from ':|id|))
         (username (getf from ':|username|))
         (first-name (getf from ':|first_name|))
         (is-bot (getf from ':|is_bot|))
         (language-code (getf from ':|language_code|)))
    (log:info "Store user data into DB.")
    (let ((attributes '(from_id username first_name is_bot language_code))
          (values `(,from-id ,username ,first-name ,is-bot ,language-code)))
      (clsql:insert-records
       :into 'users
       :attributes attributes
       :values values)))
  (declare (ignorable text))
  (cl-telegram-bot:reply "Your user data are now stored in the DB."))

(defun main ()
  (cl-telegram-bot:start-processing *gendo-bot*))
