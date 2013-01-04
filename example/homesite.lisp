;;;; homesite.lisp
;;;;
;;;; This file is part of the restas-directory-publisher library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(asdf:operate 'asdf:load-op '#:restas-directory-publisher)
(asdf:operate 'asdf:load-op '#:hunchentoot-cgi)
(asdf:operate 'asdf:load-op '#:cl-who)

(restas:define-module #:homesite
  (:use #:cl))

(in-package #:homesite)

;; homedir

(restas:mount-module -homedir- (#:restas.directory-publisher)
  (:url "/home/")
  (restas.directory-publisher:*directory* (user-homedir-pathname))
  (restas.directory-publisher:*autoindex* t))

;; gitweb

(defclass cgi-handler () ())

(defmethod restas:render-object ((renderer cgi-handler) (file pathname))
  (cond
    ((and (string= (pathname-type file) "cgi"))
     (hunchentoot-cgi::handle-cgi-script file))
    (t
     (call-next-method))))

(restas:mount-module -gitweb- (#:restas.directory-publisher)
  (:url "/gitweb/")
  (:render-method (make-instance 'cgi-handler))
  (restas.directory-publisher:*directory* #P"/usr/share/gitweb/")
  (restas.directory-publisher:*directory-index-files* '("gitweb.cgi")))

;; index

(restas:define-route index ("/")
  (who:with-html-output-to-string (out)
    (:ul
     (:li (:a :href (restas:genurl '-homedir-.route :path "") "Home"))
     (:li (:a :href (restas:genurl '-gitweb-.route :path "") "gitweb")))))

;; start

(restas:start '#:homesite :port 8080)
