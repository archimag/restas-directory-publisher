;;;; homesite.lisp
;;;;
;;;; This file is part of the restas-directory-publisher library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(asdf:operate 'asdf:load-op '#:restas-directory-publisher)

(restas:defsite #:homesite
 (:use #:cl))

(in-package #:homesite)

(restas:define-site-plugin tmp (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("tmp"))
  (restas.directory-publisher:*directory* #P"/tmp/"))


(restas:start-site '#:homesite :port 8080)
