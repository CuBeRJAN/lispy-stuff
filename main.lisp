(require "asdf")
(require 'uiop)

(import 'uiop:split-string 'CL-USER)


(defmacro make-vector (type)
  "Create a resizable array"
  `(make-array 0 :fill-pointer 0 :adjustable t :element-type ,type))

(defmacro take (n &key (start 0))
  "Take a list of n values"
  `(loop for i from ,start to (+ ,start (1- ,n)) collect i))

(defmacro --> (&rest data)
  "Call a list of functions as nested."
  (labels ((getlist (funcs)
             (if (cdr funcs)
                 (cons (car funcs) (list (getlist (cdr funcs))))
                 (if (listp (car funcs))
                     (car funcs)
                     (list (car funcs))))))
    (getlist data)))

;; This macro is inspired by clojure
(defmacro -> (&rest args)
  "Call a list of functions in order."
  (let ((data (reverse args)))
    (labels ((getlist (funcs)
               (if (cadr funcs)
                   (if (listp (car funcs))
                       (append (list (caar funcs)) (list (getlist (cdr funcs))) (cdar funcs))
                       (append (list (car funcs)) (list (getlist (cdr funcs)))))
                   (car funcs))))
      (getlist data))))

;; This one too
(defmacro ->> (&rest args)
  "Call a list of functions in order."
  (let ((data (reverse args)))
    (labels ((getlist (funcs)
               (if (cdr funcs)
                   (if (listp (car funcs))
                       (append (car funcs) (list (getlist (cdr funcs))))
                       (append (list (car funcs)) (list (getlist (cdr funcs)))))
                   (car funcs))))
      (getlist data))))

(defun vector-push-resize (var vec)
  "Push to the end of vector."
  (adjust-array vec (1+ (array-dimension vec 0)))
  (vector-push var vec))

(defun vector-append (vec1 vec2)
  "Append vector to the end of another vector"
  (adjust-array vec1 (+ (array-dimension vec1 0) (array-dimension vec2 0)))
  (dotimes (n (array-dimension vec2 0))
    (vector-push (aref vec2 n) vec1)))

(defun remove-nth (index data)
  "Remove element from list by index."
  (labels ((remove-nth-tail (index data final)
             (if (zerop index)
                 (if (not (cdr data))
                     (reverse final)
                     (revappend final (cdr data)))
                 (remove-nth-tail (1- index) (cdr data) (cons (car data) final)))))
    (remove-nth-tail index data nil)))

(defun delete-nth (i seq)
  "Delete nth element from sequence."
  (let ((slide (subseq seq (1+ i)))
        (num (1- (fill-pointer seq))))
    (replace seq slide :start1 i)
    (adjust-array seq num
                  :fill-pointer num)))

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
  (let ((fstr (make-array '(0) :element-type 'character
                               :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s fstr)
      (uiop:run-program cmd :output s))
    fstr))

(defun run-cmd (cmd)
  (uiop:run-program cmd))

(defun run-cmd-with-output (cmd)
  (uiop:run-program cmd :output t))

(defun join-strings (data &key (separator " "))
  "Convert a list of strings into a single string."
  (labels ((join-strings-tail (data separator final)
             (if (cdr data)
                 (join-strings-tail (cdr data) separator (concatenate 'string final (car data) separator))
                 (concatenate 'string final (car data)))))
    (join-strings-tail data separator nil)))

(defun sbcl-compile-executable (func out)
  "Compile a function into an executable."
  (sb-ext:save-lisp-and-die out
                            :executable t
                            :toplevel func))
