;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(defpackage :hu.dwim.asdf
  (:use :asdf
        :common-lisp)

  (:export #:system-pathname
           #:system-relative-pathname
           #:system-package-name
           #:system-test-system-name
           #:system-test-result
           #:*load-as-production?*
           #:debug-only
           #:debug-only*
           #:production-only
           #:production-only*
           #:optimize-declaration))
