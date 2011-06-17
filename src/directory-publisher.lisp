;;;; restas-direcotry-publisher.lisp
;;;;
;;;; This file is part of the restas-directory-publisher library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(restas:define-module #:restas.directory-publisher
  (:use :cl :iter)
  (:export #:route
           #:*directory*
           #:*directory-index-files*
           #:*autoindex*
           #:*enable-cgi-by-type*
           #:*ignore-pathname-p*
           #:default-pathname-info
           #:*pathname-info*
           #:hidden-pathname-p

           #:view))

(in-package #:restas.directory-publisher)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; compile template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (closure-template:compile-cl-templates
   (merge-pathnames "src/directory-publisher.tmpl"
                    (asdf:system-source-directory '#:restas-directory-publisher))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; module params
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *directory* nil)

(defvar *directory-index-files* '("index.html" "index.htm"))

(defvar *autoindex* t)

(defvar *enable-cgi-by-type* nil)

(defvar *ignore-pathname-p* nil)

(defvar *pathname-info* nil)

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
      (car (last (cdr (pathname-directory path))))
      (let ((name (pathname-name path))
            (type (pathname-type path)))
        (if type
            (format nil "~A.~A" name type)
            name))))

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


(defun default-pathname-info (path)
  "Information on pathname as plist"
  (let* ((stat (isys:stat (native-namestring path)))
         (last-modified (local-time:format-timestring nil
                                                      (local-time:unix-to-timestamp (isys:stat-mtime stat))
                                                      :format '((:day 2) #\- :short-month #\- :year #\Space (:hour 2) #\: (:min 2))))
         (dir (fad:directory-pathname-p path)))
    (list :mime-type (if (not dir)
                         (hunchentoot:mime-type path))
          :name (path-last-name path)
          :last-modified last-modified
          :size (if (not dir)
                    (format-size (isys:stat-size stat))))))

(defun pathname-info (path)
  (if *pathname-info*
      (funcall *pathname-info* path)
      (default-pathname-info path)))

(defun hidden-pathname-p (path)
  (if (not (equal path
                  (make-pathname :directory (list :absolute))))
      (char= (char (path-last-name path) 0)
             #\.)))

(defun ignore-pathname-p (path)
  (if *ignore-pathname-p*
      (funcall *ignore-pathname-p* path)
      (hidden-pathname-p path)))

(defun directory-autoindex-info (path rpath)
  "Info on directory for autoindex"
  (labels ((sort-by-name (seq)
             (sort seq
                   #'(lambda (a b)
                       (string< (getf a :name)
                                (getf b :name))))))
    (let (directories files)
      (iter (for item in (fad:list-directory (merge-pathnames path *directory*)))
            (unless (ignore-pathname-p item)
              (if (fad:directory-pathname-p item)
                  (push item directories)
                  (push item files))))
      (list :title (format nil "Index of /~A" rpath)
            :curdir (or (car (last (cdr (pathname-directory rpath))))
                        "/")
            :parents (if (not (equal path *directory*))
                         (cons (list :href (restas:genurl 'route
                                                          :path '("")
                                                          :name "/"))
                               (iter (for item in (butlast (cdr (pathname-directory rpath))))
                                     (collect item into curpath)
                                     (collect (list :href (restas:genurl 'route
                                                                         :path (append curpath
                                                                                       '("")))
                                                    :name item)))))
            :directories (sort-by-name (iter (for item in directories)
                                             (collect (list* :href (restas:genurl 'route
                                                                                  :path (append (cdr (pathname-directory rpath))
                                                                                                (list (path-last-name item) "")))
                                                             (pathname-info item)))))
            :files (sort-by-name (iter (for item in files)
                                       (collect (list* :href (restas:genurl 'route
                                                                            :path (append (cdr (pathname-directory rpath))
                                                                                          (list (path-last-name item))))
                                                       (pathname-info item)))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; routes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(restas:define-route route ("*path" :method :get)
  (let* ((relative-path (parse-native-namestring (format nil "~{~A~^/~}" path)))
         (path (merge-pathnames relative-path
                                *directory*)))
    (cond
      ((find :up (pathname-directory relative-path)) hunchentoot:+http-bad-request+)
      ((ignore-pathname-p path) hunchentoot:+http-not-found+)
      ((and (fad:directory-pathname-p path)
            (fad:directory-exists-p path)) (or (iter (for index in *directory-index-files*)
                                                     (let ((index-path (merge-pathnames index path)))
                                                       (finding index-path
                                                                such-that (fad:file-exists-p index-path))))
                                               (if *autoindex*
                                                   (directory-autoindex-info path relative-path)
                                                   hunchentoot:+http-not-found+)))
      ((not (fad:file-exists-p path)) hunchentoot:+http-not-found+)
      #+sbcl ((find (pathname-type path) 
                    *enable-cgi-by-type* 
                    :test #'string=) (hunchentoot-cgi::handle-cgi-script path))
      (t path))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass view ()
  ((ttable)))

(defmethod shared-initialize :after ((view view) slot-names &key template-package)
  (setf (slot-value view 'ttable)
        (closure-template:package-ttable 
         (find-package (or template-package
                           '#:restas.directory-publisher.view)))))

(defmethod restas:render-object ((view view) (data cons))
  (if (eql (restas:route-symbol restas:*route*) 'route)
      (closure-template:ttable-call-template (slot-value view 'ttable)
                                             "AUTOINDEX"
                                             data)
      (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                             
(defmethod restas:initialize-module-instance ((module (eql #.*package*)) context)
  (unless (restas:context-symbol-value context '*default-render-method*)
    (restas:context-add-variable context
                                 '*default-render-method*
                                 (make-instance 'view))))