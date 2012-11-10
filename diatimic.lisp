;;;; diatimic.lisp

(in-package #:diatimic)

;;; "diatimic" goes here. Hacks and glory await!

(defmacro with-html (&body body)
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     ,@body))

(defparameter *main-port* 17036)

(defvar *main-acceptor* (make-instance 'hunchentoot:easy-acceptor :port *main-port*))

(defvar *loaded* (hunchentoot:start *main-acceptor*))

(defun hash-password (password)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha1
    (ironclad:ascii-string-to-byte-array password))))

(defvar *user-name-password-map* (make-hash-table :test #'equal))

(defun confirm-password (username password)
  (let ((real-password (gethash username *user-name-password-map*)))
    (equal (hash-password password) real-password)))

(defun user-in-databasep (username)
  (multiple-value-bind (_ present) (gethash username *user-name-password-map*)
    (declare (ignore _))
    present))

(defun add-user-to-database (username password-hash)
  (setf (gethash username *user-name-password-map*) (hash-password password-hash)))


(defparameter *code-dir* (concatenate 'string
                                      #+sbcl(sb-posix:getcwd)
                                      #+ccl(format nil "~a" (ccl:current-directory)
                                                   ) "/" ))

(defparameter *global-title* "diatimic, the graphing time tracker")

;; links bootstrap.css to my working dir
(push
 (hunchentoot:create-static-file-dispatcher-and-handler
  "bootstrap.css" (concatenate 'string *code-dir* "bootstrap.css"))
      hunchentoot:*dispatch-table*)

(defun add-file-to-path (file)
  (push
   (hunchentoot:create-static-file-dispatcher-and-handler
         (concatenate 'string "/" file) (concatenate 'string *code-dir* file))
        hunchentoot:*dispatch-table*))

(add-file-to-path "bootstrap.css")
(add-file-to-path "mainpage.css")
(add-file-to-path "bootstrap-responsive.css")

;; produces the cl-who code to add a spreadsheet
(defun add-spreadsheet (file)
  `(:link :type "text/css" :href ,file :rel "stylesheet"))

(defmacro std-html-page (&key title head body)
  `(with-html
     (:html :lang "en"
            (:head
             (:meta :http-equiv "Content-Type"
                    :content "text/html;charset=utf-8")
             ,(add-spreadsheet "/bootstrap.css")
             ,(add-spreadsheet "/bootstrap-responsive.css")
             ,(add-spreadsheet "/mainpage.css")
             (:title ,title)
             ,(macroexpand head))
            (:body ,@body))))

(defmacro username-password-form (command form-id)
  (macroexpand
   `(cl-who:with-html-output (*standard-output* nil :prologue nil :indent t)
      (:form :method :post
             :id ,form-id
             :onsubmit (ps-inline
                        (progn
                          (when (= (@ username value) "")
                            (alert "Please enter a username."))
                          (when (= (@ password value) "")
                            (alert "Please enter a password."))
                          (setf (@ form_id value) ,form-id)))
             (:input :type :hidden :name "form_id")
             (:p "Username")
             (:p (:input :type :text
                         :name "username"))
             (:p "Password")
             (:p (:input :type :password
                         :name "password"))
             (:p (:input 
                  :type :submit
                  :class "btn btn-primary btn-large"
                  :style "padding:14px 0px; margin-bottom:0px;width: 50%;"
                  :value ,command))))))

(defun string-empty (string)
  (< 0 (length string)))

(defun string-non-empty (string) (and string (not (string-empty string))))


(defun logout-link ()
  (cl-who:with-html-output (*standard-output*)
    (:a :href "/logout" "Logout")))

(defun main-page ()
  (std-html-page :title *global-title*
      :body
       ((:center
        (:h1 "diatimic")
        (:h2 "A clock goes here!"))
       (logout-link))))

(defmacro redirect-to-main (&key (delay-milliseconds 2000))
  (macroexpand
   `(cl-who:with-html-output (*standard-output*)
      (ps
        (set-timeout (lambda ()
                       (if (= (@ window location) "/")
                           ((@ (@ window location) reload))
                           (setf (@ window location) "/"))
                       null)
                     ,delay-milliseconds)))))

(defun login-result-page (username password)
  (if (confirm-password username password)
      (progn
        (setf (hunchentoot:session-value :username) username)
        (std-html-page :title *global-title*
                       :head (redirect-to-main)
                       :body
                       ((:center
                         (:p (format t "Welcome, ~a." username))))))
      (failed-login-page username)))

(defun redir-uri (uri)
  (hunchentoot:redirect uri))

(defparameter *login-success-page* "/login-success")
(defun login-success-page ()
  (std-html-page :title *global-title*
                       :head (redirect-to-main)
                       :body
                       ((:center
                         (:p (format t "Welcome, ~a." (hunchentoot:session-value :username)))))))

(defparameter *login-failure-page* "/login-failure")
(defun login-failure-page ()
  (std-html-page :title *global-title*
                 :head (redirect-to-main)
                 :body
                 ((:center
                   (:p (format t "Sorry, could not log in."))))))

(defun login-check (username password)
  (if (confirm-password username password)
      (progn
        (setf (hunchentoot:session-value :username) username)
        (redir-uri *login-success-page*))
      (redir-uri *login-failure-page*)))

(defparameter *register-success-page* "/register-success")
(defun register-success-page ()
  (std-html-page :title *global-title*
                       :body
                       ((:center
                        (:h1 (format t "Welcome, ~a. Your account was created successfully."
                                     (hunchentoot:session-value :username)))))))

(defparameter *register-failure-page* "/register-failure")
(defun register-failure-page ()
  (std-html-page :title *global-title*
                     :head (redirect-to-main)
                     :body
                     ((:center
                       (:h1 (format t "Sorry, that username is already in use."))))))

(defun register-result-page (username password)
  (if (user-in-databasep username)
      (register-failure-page)
      ;; (redir-uri *register-failure-page*)
      (progn
        (add-user-to-database username password)
        (setf (hunchentoot:session-value :username) username)
        (register-success-page))))

(defun login-or-register-page ()
  (std-html-page :title *global-title*
                 :body ((:center
                         (:h1 "Welcome to diatimic, the graphing time tracker!"))
                        (:center
                         (:p)
                         (:p "Please login or register")
                         (username-password-form "Login" "login-form")
                         (username-password-form "Register" "register-form")))))


(defun main-login-page ()
  (let ((used-form (hunchentoot:post-parameter "form_id"))
        (username (hunchentoot:post-parameter "username"))
        (password (hunchentoot:post-parameter "password")))
    (hunchentoot:no-cache)
    (cond
      ((hunchentoot:session-value :username)
       (main-page))
      ((and (string= used-form "login-form") username password)
       (login-result-page username password))
      ((and (string= used-form "register-form") username password)
       (register-result-page username password))
      (t (login-or-register-page)))))

(defun logout-command ()
  (setf (hunchentoot:session-value :username) nil)
  (hunchentoot:redirect "/"))

(defparameter *dispatch-table*
  (list
   (hunchentoot:create-regex-dispatcher "^/$" #'main-login-page)
   (hunchentoot:create-regex-dispatcher "^/logout$" #'logout-command)
   (hunchentoot:create-regex-dispatcher
    (concatenate 'string "^" *login-success-page* "$") #'login-success-page)
   (hunchentoot:create-regex-dispatcher
    (concatenate 'string "^" *login-failure-page* "$") #'login-failure-page)
   (hunchentoot:create-regex-dispatcher
    (concatenate 'string "^" *register-success-page* "$") #'register-success-page)
   (hunchentoot:create-regex-dispatcher
    (concatenate 'string "^" *register-failure-page* "$") #'register-failure-page)))
