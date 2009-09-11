;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

;;;;;;
;;; Builds standalone images for development and production environments

(in-package :hu.dwim.asdf)

;;;;;;
;;; Util

(defvar +development-image-suffix+ ".development")

(defun ensure-external-format-is-utf-8 ()
  #+sbcl
  (unless (eq (sb-impl::default-external-format) :utf-8)
    (cerror "Ignore" "The default external format is ~S, but UTF-8 is strongly advised! Check your $LANG env variable..."
            (sb-impl::default-external-format))))

(defun load-swank ()
  (require :swank)
  (unless (find-symbol "SWANK-PROFILE-GET-CALL-GRAPH" (find-package "SWANK-BACKEND"))
    (load (merge-pathnames "hu.dwim.environment/source/swank-sprof.lisp" common-lisp-user::*workspace-directory*)))
  (when (ignore-errors
          (fdefinition (read-from-string "(setf swank:swank-print-right-margin)")))
    (eval (read-from-string "(setf (swank:swank-print-right-margin) 150
                                   swank:*globally-redirect-io* t)"))))

(defun save-image (file-name &rest args &key &allow-other-keys)
  (ensure-external-format-is-utf-8)
  (in-package :common-lisp-user)
  (when (probe-file file-name)
    (delete-file file-name))
  #+sbcl
  (apply #'sb-ext:save-lisp-and-die file-name args))

(defun find-all-systems-with-prefix (system-name-prefix)
  (dolist (dir *central-registry*)
    (dolist (file (directory (merge-pathnames (eval dir) (make-pathname :name :wild :type "asd"))))
      (let ((name (pathname-name file)))
        (when (search system-name-prefix name)
          (find-system name))))))

;;;;;;
;;; Build image

(defun build-development-image (system-name-prefix)
  (let ((file-name (merge-pathnames (concatenate 'string "hu.dwim.environment/core/"
                                                 (string-downcase system-name) +development-image-suffix+ ".core")
                                    common-lisp-user::*workspace-directory*))
        (development-systems nil))
    (load-swank)
    (find-all-systems-with-prefix system-name-prefix)
    (maphash (lambda (key value)
               (when (eq 0 (search system-name-prefix key))
                 (let ((system (cdr value)))
                   (pushnew system development-systems)
                   (dolist (specification (component-depends-on 'load-op system))
                     (when (eq 'load-op (first specification))
                       (map nil (lambda (name)
                                  (unless (search system-name-prefix (component-name (find-system name)))
                                    (load-system name)))
                            (cdr specification)))))))
             asdf::*defined-systems*)
    (map nil (lambda (system)
               (remhash (component-name system) asdf::*defined-systems*))
         development-systems)
    (save-image file-name)))

(defun build-production-image (system-name)
  (let ((file-name (system-relative-pathname system-name system-name))
        (*load-as-production?* t))
    (setf *default-toplevel-directory* (merge-pathnames "../.fasls/" common-lisp-user::*workspace-directory*))
    (load-swank)
    (load-system system-name)
    (save-image file-name
                :executable t
                :save-runtime-options t
                :toplevel (or (find-symbol "PRODUCTION-IMAGE-TOPLEVEL" (hu.dwim.asdf:system-package-name (find-system system-name)))
                              #+sbcl #'sb-impl::toplevel-init
                              #-sbcl (lambda () (error "No production image toplevel function"))))))

(defun build-image (command-line-arguments)
  (let* ((arguments (rest command-line-arguments))
         (system-name (first arguments)))
    (if (search +development-image-suffix+ system-name)
        (build-development-image (subseq system-name 0 (- (length system-name) (length +development-image-suffix+))))
        (build-production-image system-name))))