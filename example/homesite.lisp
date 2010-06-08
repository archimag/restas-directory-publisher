;;;; homesite.lisp
;;;;
;;;; This file is part of the restas-directory-publisher library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(asdf:operate 'asdf:load-op '#:restas-directory-publisher)

(restas:start '#:restas.directory-publisher 
              :port 8080 
              :context (restas:make-context (restas.directory-publisher:*baseurl* '("tmp"))
                                            (restas.directory-publisher:*directory* #P"/tmp/")
                                            (restas.directory-publisher:*autoindex* t)))