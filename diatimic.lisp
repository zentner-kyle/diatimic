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

(defparameter *code-dir* (concatenate 'string
                                      #+sbcl(sb-posix:getcwd)
                                      #+ccl(format nil "~a" (ccl:current-directory)
                                                   ) "/" ))

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


(defun confirm-password (username password)
  (print "Confirming password!")
  (let ((real-password (gethash username *user-name-password-map*)))
    (equal (hash-password password) real-password)))

;; produces the cl-who code to add a spreadsheet
(defun add-spreadsheet (file)
  `(:link :type "text/css" :href ,file :rel "stylesheet"))

(defmacro std-html-page (title &body body)
  `(with-html
     (:html :lang "en"
            (:head
             (:meta :http-equiv "Content-Type"
                    :content "text/html;charset=utf-8")
             ,(add-spreadsheet "/bootstrap.css")
             ,(add-spreadsheet "/bootstrap-responsive.css")
             ,(add-spreadsheet "/mainpage.css")
             (:title ,title))
            ,@body)))

(defun user-in-databasep (username)
  (format t "~a" username))


(defmacro username-password-form (command form-id)
  (macroexpand
   `(cl-who:with-html-output (*standard-output* nil :prologue nil :indent t)
      (:form :method :post
             :id ,form-id
             :onsubmit (ps:ps-inline
                        (progn
                          (when (= (ps:@ username value) "")
                            (alert "Please enter a username."))
                          (when (= (ps:@ password value) "")
                            (alert "Please enter a password."))
                          ;; (alert (+ "form id:" (ps:@ form_id value) ""))
                          (setf (ps:@ form_id value) ,form-id)))
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

(defmacro failed-login-page (username password used-form)
  `(progn
     (std-html-page :title "diatimic: Could not log in!"
                          (:body (:center
                                  (:p (format t "form: ~a" ,used-form))
                                  (:p (format t "Sorry, could not log in as ~a" ,username)))))
     ;; (setf ,username nil)
     ;; (setf ,password nil)
     ;; (setf ,used-form nil)
     ))

(hunchentoot:define-easy-handler (main-login-page :uri "/") ()
  (print "TEST")
  (let ((title "diatimic, the graphing time tracker")
        (used-form (hunchentoot:post-parameter "form_id"))
        (username (hunchentoot:post-parameter "username"))
        (password (hunchentoot:post-parameter "password")))
    (hunchentoot:no-cache)
    (cond
      ((and (string= used-form "login-form") username password)
       (if (confirm-password username password)
           (progn
             (setf (hunchentoot:session-value 'username) username)
             (std-html-page title
               (:body
                (:center
                 (:p (format t "Welcome, ~a." username))))))
           (failed-login-page username password used-form)))
      ((and (string= used-form "register-form") username password)
       (if (user-in-databasep username) nil))
      (t (std-html-page :title title
                        (:body (:center
                                (:h1 "Welcome to diatimic, the graphing time tracker!"))
                               (:center
                                (:p)
                                (:p "Please login or register")
                                (username-password-form "Login" "login-form")
                                (username-password-form "Register" "register-form"))))))))

