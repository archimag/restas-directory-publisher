;;;; homesite.lisp

(asdf:operate 'asdf:load-op '#:restas-directory-publisher)

(restas:defsite #:homesite
 (:use #:cl))

(in-package #:homesite)

(restas:define-site-plugin tmp (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("tmp"))
  (restas.directory-publisher:*directory* #P"/tmp/"))


(restas:start-site '#:homesite :port 8080)
