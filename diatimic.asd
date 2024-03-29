;;;; diatimic.asd

(asdf:defsystem #:diatimic
  :serial t
  :description "A web app for productivity."
  :author "Andrew Chen <andrewtheannihilator@gmail.com> and Kyle Zentner <zentner.kyle@gmail.com>"
  :license "BSD"
  :depends-on (#:iterate
               #:vecto
               #:cl-who
               #:ironclad
               #:parenscript
               #:cl-fad
               #:hunchentoot)
  :components ((:file "package")
               (:file "diatimic")))

