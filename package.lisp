;;;; package.lisp

(defpackage #:diatimic
  (:use #:cl)
  ;; (:shadowing-import-from :cl-who
  ;;                         :str :with-html-output)
  (:export :*code-dir*
           :*message-log-pathname*)
  )

