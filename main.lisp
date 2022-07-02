#!/usr/bin/sbcl --script

;; Enable quicklisp
(defun load-quicklisp ()
  #-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init))))

(require "asdf")


(defun get-cmd-output (cmd)
  "get output of command as a string"
  (let ((fstr (make-array '(0) :element-type 'base-char
                               :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s fstr)
      (uiop:run-program cmd :output s))
    fstr))

(defun run-cmd (cmd)
  (uiop:run-program cmd))

(defun get-argv-string ()
  "get command line arguments as a string"
  (let ((fstr (make-array '(0) :element-type 'base-char
                               :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s fstr)
      (format s "~{~A~^ ~}" (cdr *posix-argv*)))
    fstr))


(format t "~a~%" (get-argv-string))
