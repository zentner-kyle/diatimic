;;;; diatimic.lisp

(in-package #:diatimic)

;;; "diatimic" goes here. Hacks and glory await!

(defclass time ()
  ())

(defclass event ()
  ((time :initarg :time)))

