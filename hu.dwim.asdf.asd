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
                :components ((:file "package")
                             (:file "production" :depends-on ("workspace"))
                             (:file "system" :depends-on ("production"))
                             (:file "workspace" :depends-on ("package"))))))
