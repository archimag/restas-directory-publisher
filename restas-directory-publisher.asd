;;;; restas-extra.asd

(defpackage #:restas-directory-publisher-system
  (:use #:cl #:asdf))

(in-package #:restas-directory-publisher-system)

(defsystem restas-directory-publisher
  :depends-on (#:restas #:closure-template #:local-time)
  :components ((:module "src"
                        :components ((:file "directory-publisher")))))
