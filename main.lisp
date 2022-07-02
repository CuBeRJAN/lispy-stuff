#!/usr/bin/sbcl --script
(require "asdf")


(defun load-quicklisp ()
  "Initialize quicklisp."
  #-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init))))

(defun get-argv ()
  "Return a list of command line arguments."
  (or
   #+CLISP *args*
   #+SBCL (cdr *posix-argv*)
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

(defun replace-substring (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

(defun get-cmd-output (cmd)
  "Returns output of shell command as a string."
  (let ((fstr (make-array '(0) :element-type 'base-char
                               :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s fstr)
      (uiop:run-program cmd :output s))
    fstr))

(defun run-cmd (cmd)
  (uiop:run-program cmd))

(defun list-to-string (data separator)
  "Convert a list of strings into a single string."
  (let ((fstr ""))
    (loop for i from 0 to (- (length data) 1) do
      (setq fstr (concatenate 'string fstr (nth i data) separator)))
    fstr))

(defun sbcl-compile-executable (func out)
  "Compile a function into an executable."
  (sb-ext:save-lisp-and-die out
                            :executable t
                            :toplevel func))
