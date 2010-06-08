;;;; restas-directory-publisher.asd
;;;;
;;;; This file is part of the restas-directory-publisher library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage #:restas-directory-publisher-system
  (:use #:cl #:asdf))

(in-package #:restas-directory-publisher-system)

(defsystem restas-directory-publisher
  :depends-on (#:restas #:closure-template #:local-time #:iolib.syscalls #+sbcl :hunchentoot-cgi)
  :components ((:module "src"
                        :components ((:file "directory-publisher")))))
