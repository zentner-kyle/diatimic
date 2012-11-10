;;;; diatimic.lisp

(in-package #:diatimic)

;;; "diatimic" goes here. Hacks and glory await!

(defmacro with-html (&body body)
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     ,@body))

(defparameter *main-port* 9001)


(defvar *main-acceptor* (make-instance 'hunchentoot:easy-acceptor :port *main-port*))

(defvar *loaded* (hunchentoot:start *main-acceptor*))

(defun hash-password (password)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha1
    (ironclad:ascii-string-to-byte-array password))))

(defvar *user-name-password-map* (make-hash-table))

(defparameter *code-dir* (concatenate 'string (sb-posix:getcwd) "/" ))


(defun add-file-to-path (file)
  (push
   (hunchentoot:create-static-file-dispatcher-and-handler
    (concatenate 'string "/" file) (concatenate 'string *code-dir* file))
   hunchentoot:*dispatch-table*))

(add-file-to-path "bootstrap.css")
(add-file-to-path "mainpage.css")
(add-file-to-path "bootstrap-responsive.css")
(add-file-to-path "jquery-latest.js")

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
             (:link :type "text/css" :href "/bootstrap-responsive.css" :rel "stylesheet")
             (:link :type "text/css" :href "/mainpage.css" :rel "stylesheet")
             (:script :src "/jquery-latest.js")
             (:title ,title))
            ,@body)))

(hunchentoot:define-easy-handler (main-login-page :uri "/") ()
  (let ((title "diatimic, the graphing time tracker")
        (username (hunchentoot:post-parameter "username"))
        (password (hunchentoot:post-parameter "password")))
    (hunchentoot:no-cache)
    (std-html-page :title title
                   (:script :type "text/javascript"
                            (cl-who:str
                             (ps ((@ ($ document) ready)
                                  (lambda ()
                                        ;function starts here
                                        ;(+ 1 2)
                                    (progn
                                      (setf time 0)
                                      (setf stopped true)
                                      ((@ window set-interval) periodic-task 1000)
                                      ((@ ($ "div.second") append)
                                       "<center class=added ><h1>0</h1></center>")
                                      (+ 1 2)
                                      nil


                                      )))
                                 (defun periodic-task ()
                                   ((@ console log) "Periodic Task called")
                                   (if (not stopped)
                                       (progn
                                         (setf time (+ 1 time))
                                         ((@ ($ ".added") remove ))
                                         ((@ ($ "div.second") append)
                                          (concatenate 'string
                                                       "<center class=added><h1>"
                                                       time
                                                       "</h1></center>"))
                                         nil)))
                                 ; pull the h1 up and down
                                 (defun act-press ()
                                   (progn
                                     ((@
                                       ((@
                                         ((@ ($ "h1.diatimic") slide-up) 300)
                                         delay) 1000)
                                       slide-down) 300)
                                     (setf stopped (not stopped))
                                     nil
                                     )
                                   )

                                 )
                             )
                            )
                   (:body (:center
                           (:h1 :class "diatimic" "Welcome to diatimic, the graphing time tracker!"))
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
                          (:a :class "btn btn-primary btn-large" :style "padding:14px 0px; margin-bottom:0px;width: 50%;" :href "#" :onclick (ps (login-callback))
                              "Login")
                          (:br )
                          (:br )
                          (:a :class "btn btn-primary btn-large" :style "padding:14px 0px; margin-bottom:0px;width: 50%;" :href "#" :onclick (ps (act-press))
                              "Act")
                          (:div :class "second")

))))
