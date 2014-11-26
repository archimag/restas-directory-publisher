;;;; restas-directory-publisher.asd
;;;;
;;;; This file is part of the restas-directory-publisher library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem #:restas-directory-publisher
  :defsystem-depends-on (#:closure-template)
  :depends-on (#:restas.core #:local-time)
  :pathname "src"
  :serial t
  :components ((:closure-template "autoindex")
               (:file "directory-publisher")))
