;;;; restas-direcotry-publisher.lisp
;;;;
;;;; This file is part of the restas-directory-publisher library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(restas:define-module #:restas.directory-publisher
  (:use :cl :iter)
  (:export #:*directory*
           #:*directory-index-files*
           #:*autoindex*
           #:*autoindex-template*
           #:*enable-cgi-by-type*))

(in-package #:restas.directory-publisher)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; compile template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (closure-template:compile-template :common-lisp-backend
                                     (merge-pathnames "src/directory-publisher.tmpl"
                                                      (asdf:component-pathname (asdf:find-system '#:restas-directory-publisher)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Plugin variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *directory* nil)

(defvar *directory-index-files* '("index.html" "index.htm"))

(defvar *autoindex* t)

(defvar *autoindex-template* 'restas.directory-publisher.view:autoindex)

(defvar *enable-cgi-by-type* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; native namestrings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun native-namestring (path)
  #+sbcl (sb-ext:native-namestring path)
  #-sbcl (namestring path))

(defun parse-native-namestring (thing)
  #+sbcl (sb-ext:parse-native-namestring thing)
  #-sbcl (parse-namestring thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; directory info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun path-last-name (path)
  "File or directory name"
  (if (fad:directory-pathname-p path)
      (car (last (pathname-directory path)))
      (let ((name (pathname-name path))
            (type (pathname-type path)))
        (if type
            (format nil "~A.~A" name type)
            name))))

(defun hidden-file-p (path)
  (char= (char (path-last-name path)
               0)
         #\.))

(defparameter *byte-units* '("kB" "MB" "GB" "TB" "PB" "EB" "ZB" "YB")
  "Symbols for show size of files in human readable format")

(defun format-size (bytes)
  "Convert number of bytes to human readable format (e.g. 1K 234M 2G)"
  (let* ((unit (if (> bytes 0)
                   (floor (log bytes 1024))
                   bytes))
         (symbol (if (> unit 0)
                     (nth (1- unit)
                          *byte-units*))))
    (if symbol
        (format nil
                "~,1F ~A"
                (/ bytes (expt 1024 unit))
                symbol)
        (format nil "~A B" bytes))))


(defun path-info (path)
  "Information on pathname as plist"
  (let* ((stat (isys:stat (native-namestring path)))
         (last-modified (local-time:format-timestring nil
                                                      (local-time:unix-to-timestamp (isys:stat-mtime stat))
                                                      :format '((:day 2) #\- :short-month #\- :year #\Space (:hour 2) #\: (:min 2))))
         (dir (fad:directory-pathname-p path)))
    (list :type (if dir
                    "Directory"
                    (hunchentoot:mime-type path))
          :name (path-last-name path)
          :last-modified last-modified
          :size (if (not dir)
                    (format-size (isys:stat-size stat))))))

(defun directory-autoindex-info (path rpath)
  "Info on directory for autoindex"
  (list :title (format nil "Index of /~A" rpath)
        :parent (unless (equal path *directory*) "..")
        :paths (iter (for item in (fad:list-directory (merge-pathnames path *directory*)))
                     (unless (hidden-file-p item)
                       (collect (path-info item))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; routes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(define-route route ("*path" :method :get)
  (let* ((relative-path (parse-native-namestring (format nil "~{~A~^/~}" path)))
         (path (merge-pathnames relative-path
                                *directory*)))
    (cond
      ((find :up (pathname-directory relative-path)) hunchentoot:+http-bad-request+)
      ((and (fad:directory-pathname-p path)
            (fad:directory-exists-p path)) (or (iter (for index in *directory-index-files*)
                                                     (let ((index-path (merge-pathnames index path)))
                                                       (finding index-path
                                                                such-that (fad:file-exists-p index-path))))
                                               (if *autoindex*
                                                   (funcall *autoindex-template*
                                                            (directory-autoindex-info path relative-path))
                                                   hunchentoot:+http-not-found+)))
      ((not (fad:file-exists-p path)) hunchentoot:+http-not-found+)
      #+sbcl ((find (pathname-type path) 
                    *enable-cgi-by-type* 
                    :test #'string=) (hunchentoot-cgi::handle-cgi-script path))
      (t path))))
