;;;; gitweb.lisp
;;;;
;;;; This file is part of the restas-directory-publisher library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(asdf:operate 'asdf:load-op '#:restas-directory-publisher)

(restas:defsite #:gitweb
 (:use #:cl))

(in-package #:gitweb)

(restas:define-site-plugin tmp (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("gitweb"))
  (restas.directory-publisher:*directory* #P"/usr/share/git/gitweb/")
  (restas.directory-publisher:*enable-cgi-by-type* '("cgi")))

(restas:start-site '#:gitweb :port 8080)
