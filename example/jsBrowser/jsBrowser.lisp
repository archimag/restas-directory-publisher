;;;; jsBrowser.lisp
;;;;
;;;; This file is part of the restas-directory-publisher library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op '#:restas-directory-publisher)
  (asdf:operate 'asdf:load-op '#:closure-template)
  (asdf:operate 'asdf:load-op '#:parenscript)
  (asdf:operate 'asdf:load-op '#:cl-json))

(restas:define-module #:restas.js-browser
  (:use #:cl))

(in-package #:restas.js-browser)

(defparameter *js-browser-directory*
  (merge-pathnames "example/jsBrowser/"
                   (asdf:component-pathname (asdf:find-system '#:restas-directory-publisher))))

(defparameter *templates-path*
  (merge-pathnames "jsBrowser.tmpl"
                   *js-browser-directory*))

(defparameter *js-browser-templates*
  (closure-template:compile-template :javascript-backend
                                     *templates-path*))

(closure-template:compile-template :common-lisp-backend
                                   *templates-path*)

(defun encode-json (obj)
  (flet ((encode-json-list (list stream)
           (if (keywordp (car list))
               (json:encode-json-plist list stream)
               (json::encode-json-list-guessing-encoder list stream))))
    (let ((json::*json-list-encoder-fn* #'encode-json-list))
      (json:encode-json-to-string obj))))

(restas:define-route main ("")
  (restas.js-browser.view:main-page nil))

(restas:define-route js-browser-tempalte.js ("static/jsBrowserTemplates.js"
                                             :content-type "application/javascript")
  *js-browser-templates*)
  

(restas:mount-submodule -static- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("static"))
;;  (restas.directory-publisher:*directory* (merge-pathnames "static/" *js-browser-directory*)))
  (restas.directory-publisher:*directory* #P"/")
  (restas.directory-publisher:*autoindex* t))

(restas:mount-submodule -api- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("api"))
  (restas.directory-publisher:*directory* #P"/")
  (restas.directory-publisher:*autoindex-template* #'encode-json)
  (restas.directory-publisher:*autoindex* t))


(restas:start '#:restas.js-browser :port 8080)