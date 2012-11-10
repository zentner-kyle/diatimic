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

(defvar *user-name-password-map* (make-hash-table))

(defparameter *code-dir* (concatenate 'string (sb-posix:getcwd) "/" ))

;; links bootstrap.css to my working dir
(push
 (hunchentoot:create-static-file-dispatcher-and-handler
  "/bootstrap.css" (concatenate 'string *code-dir* "bootstrap.css"))
      hunchentoot:*dispatch-table*)

(defun confirm-password (username password)
  (let ((real-password (gethash username *user-name-password-map*)))
    (equal (hash-password password) real-password)))

(defmacro std-html-page (title &body body)
  `(with-html
     (:html :lang "en"
            (:head
             (:meta :http-equiv "Content-Type"
                    :content "text/html;charset=utf-8")
             (:link :type "text/css" :href "/bootstrap.css" :rel "stylesheet")
             (:title ,title))
            ,@body)))

(hunchentoot:define-easy-handler (main-login-page :uri "/") ()
  (let ((title "diatimic, the graphing time tracker")
        (username (hunchentoot:post-parameter "username"))
        (password (hunchentoot:post-parameter "password")))
    (hunchentoot:no-cache)
    (if (and username password)
        (if (confirm-password username password)
            (progn
              (setf (hunchentoot:session-value 'username) username)
              (std-html-page title
                (:body
                 (:center
                  (:p (format t "Welcome, ~a." username))))))
            (std-html-page :title "diatimic: Could not log in!"
                           (:body (:center
                                   (:p (format t "Sorry, could not log in as ~a" username))))))
        (std-html-page :title title
                       (:script :type "text/javascript"
                                (cl-who:str (ps:ps
                                       (defun login-callback ()
                                         (alert "Logging in!")))))
                       (:body (:center
                               (:h1 "Welcome to diatimic, the graphing time tracker!"))
                              (:p "Please login")
                              (:p "Username")
                              (:p (:form :method :post
                                         (:input :type :text
                                                 :name "username"
                                                 :value "")))
                              (:p "Password:")
                              (:p (:form :method :post
                                         (:input :type :password
                                                 :name "password"
                                                 :value "")))
                              (:a :class "btn btn-primary btn-large" :style "padding:14px 0px; margin-bottom:0px;width: 50%;" :href "#" :onclick (ps:ps (login-callback))
                                  "Login"))))))

