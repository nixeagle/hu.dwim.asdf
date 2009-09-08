;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.asdf
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain"
  :description "Various ASDF extensions"
  :components ((:module "source"
                :components ((:file "asdf" :depends-on ("production"))
                             (:file "build-image" :depends-on ("asdf"))
                             (:file "package")
                             (:file "production" :depends-on ("package"))))))
