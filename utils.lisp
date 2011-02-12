;;                                                               -*- Lisp -*-
;; utils.lisp -- Common utilities functions and macros
;;
;; Copyright (C) 2009,2011 David Vazquez
;; Copyright (C) 2010,2011 Mario Castelan Castro
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
(in-package :taskbot)

;;;; Misc macros

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbolize (symbol1 symbol2)
    (intern (concatenate 'string (string symbol1) (string symbol2)))))

(defmacro while (condition &body code)
  `(do ()
       ((not ,condition))
     ,@code))

(defmacro with-gensyms ((&rest vars) &body code)
  `(let ,(loop for i in vars
	       collect (etypecase i
			 (symbol `(,i (gensym ,(symbol-name i))))
			 (list `(,(first i) (gensym ,(second i))))))
     ,@code))

;;; The macro with-collectors provides a confortable way to build
;;; lists iteratively and efficiently. NAMES is a list of collector
;;; descriptors. For each collector, you have a variable which
;;; contains a list and an unary function to collect values at the end
;;; of the list efficiently.
;;;
;;; Each collector descriptor is one of the following forms:
;;;
;;;   NAME
;;;   (NAME INITFORM)
;;;   (NAME INITFORM FNAME)
;;;
;;; NAME is the name of the variable. INITFORM is a form which
;;; evaluates to the initial content of NAME, by default is NIL.
;;; FNAME is the name of the collector function name, by default is
;;; COLLECT-NAME.
;;;
;;; NOTE: You must not change the list destructively if you want that
;;; collector function works.
;;;
(defmacro with-collectors ((&rest names) &body code)
  (let ((names (mapcar #'mklist names))
        ;; A list of lists of the form (NAME INITFORM BEGIN END FNAME),
        ;; where BEGIN and END are the gensymed symbols of the
        ;; first and the last cons of the collector. Note we use a
        ;; special header cons.
        (table nil))
    ;; Fill the table
    (dolist (collector names)
      (destructuring-bind (name &optional initform fname) collector
        (push (list name
                    initform
                    (gensym)
                    (gensym)
                    (or fname (symbolize 'collect- name)))
              table)))
    (macrolet (;; Map through collectors binding NAME INITFORM BEGIN
               ;; and END variables, collecting the results in a list.
               (map* (form)
                 `(loop for (name initform begin end fname)
                        in table
                        collect ,form)))
      ;; Macroexpansion
      `(let ,(map* `(,begin (cons :collector ,initform)))
         (let ,(map* `(,end (last ,begin)))
           (symbol-macrolet ,(map* `(,name (cdr ,begin)))
             (flet ,(map* `(,fname (value)
                                   (setf (cdr ,end) (list value))
                                   (setf ,end (cdr ,end))
                                   (cdr ,begin)))
               ,@code)))))))

;;; Run CODE which an anonmoys collector which a collector function
;;; named `collect'.
(defmacro with-collect (&body code)
  (with-gensyms (name)
    `(with-collectors ((,name nil collect))
       ,@code
       ,name)))


;;;; Declarations and definitions facilities

;;; Define a predicate named NAME in order to check if the type of an
;;; object is TYPE.
;;;
;;; If type is a symbol, then NAME could be omitted. In this case,
;;; NAME is computed appending 'P' to type, or '-P' if the symbol name
;;; contains the character '-' already.
;;;
;;; Examples:
;;;   o If type is FOO, NAME will be FOOP
;;;   o If type is FOO-BAR, NAME wil be FOO-BAR-P
;;;
(defmacro define-predicate-type (type &optional name)
  (declare (type (or symbol null) name))
  (let ((fname name))
    ;; If NAME is ommited and TYPE is a symbol, then compute the
    ;; default value.
    (when (and (symbolp type) (not fname))
      (if (find #\- (string type))
          (setf fname (symbolize type "-P"))
          (setf fname (symbolize type "P"))))
    (when (null fname)
      (error "The argument NAME must be specified."))
    `(defun ,fname (x)
       (typep x ',type))))


;;; Sequences

(defun split-string (string &optional (separators " ") (omit-nulls t))
  (declare (type string string))
  (flet ((separator-p (char)
           (etypecase separators
             (character (char= char separators))
             (sequence  (find char separators))
             (function  (funcall separators char)))))
    (loop for start = 0 then (1+ end)
          for end = (position-if #'separator-p string :start start)
          as seq = (subseq string start end)
          unless (and omit-nulls (string= seq ""))
          collect seq
          while end)))

;;; Concatenate strings.
(defun concat (&rest strings)
  (if (null strings)
      (make-string 0)
      (reduce (lambda (s1 s2)
                (concatenate 'string s1 s2))
              strings)))

;;; Concatenate the list of STRINGS.
(defun join-strings (strings &optional (separator #\space))
  (if (null strings)
      (make-string 0)
      (reduce (lambda (s1 s2)
                (concatenate 'string s1 (string separator) s2))
              strings)))

;;; Return X if it is a list, (list X) otherwise.
(defun mklist (x)
  (if (listp x)
      x
      (list x)))

;;; Check if X is a list of an element.
(defun singlep (x)
  (and (consp x) (null (cdr x))))

;;; If X is a single list, it returns the element. Otherwise, return
;;; the list itself.
(defun unlist (x)
  (if (singlep x)
      (car x)
      x))


;;;; Streams

;;; Read characters from STREAM until it finds a char of CHAR-BAG. If
;;; it finds a NON-EXPECT character, it signals an error. If an end of
;;; file condition is signaled and EOF-ERROR-P is nil, return nil.
;;;
;;; Both CHAR-BAG and NOT-EXPECT could be a character, a sequence of
;;; characters or a predicate function.
;;;
(defun read-until (stream char-bag &key not-expect (eof-error-p t))
  (flet (;; Check if CH is a terminal char
         (terminal-char-p (ch)
           (etypecase char-bag
             (character (char= ch char-bag))
             (sequence  (find ch char-bag :test #'char=))
             (function  (funcall char-bag ch))))
         ;; Check if CH is not an expected char
         (not-expect-char-p (ch)
           (etypecase not-expect
             (character (char= ch not-expect))
             (sequence (find ch not-expect :test #'char=))
             (function (funcall not-expect ch)))))
    ;; Read characters
    (with-output-to-string (out)
      (loop for ch = (peek-char nil stream eof-error-p)
            until (and (not eof-error-p) (null ch))
            until (terminal-char-p ch)
            when (not-expect-char-p ch)
            do (error "Character ~w is not expected." ch)
            do (write-char (read-char stream) out)))))


;;;; Others

;;; Set PLACE to zero.
;;; This function is thought to use this function as default-value in
;;; optional or keyword arguments.
(defun required-arg ()
  (error "A required &KEY or &OPTIONAL argument was not supplied."))

;;; Iterate across entries in a hash table.
(defmacro do-hash-table ((key value) hash-table &body code)
  (with-gensyms (iter morep)
    `(with-hash-table-iterator (,iter ,hash-table)
       (loop
        (multiple-value-bind (,morep ,key ,value)
            (,iter)
          (declare (ignorable ,key ,value))
          (unless ,morep (return))
          ((lambda () ,@code)))))))

;;; Anaphoric IF.
(defmacro aif (condition then &optional else)
  `(let ((it ,condition))
     (if it ,then ,else)))

;;; Return a random element of the sequence SEQ.
(defun random-element (seq)
  (elt seq (random (length seq))))


;;; Return T if STRING1 is alphabetically before of STRING2. This
;;; function work with the char<, so it will work only for ASCII
;;; probably.
(defun alphabetically<= (string1 string2)
  (loop for c1 across (string-upcase string1)
        for c2 across (string-upcase string2)
        when (char< c1 c2) do
        (return t)
        when (char< c2 c1) do
        (return nil)
        finally
        (return t)))


;;; Check if C1 and C2 are the same character case-insensitively.
(defun char-ci= (c1 c2)
  (declare (character c1 c2))
  (char= (char-upcase c1)
         (char-upcase c2)))

;;; Check if STR1 and STR2 are the the same string case-insensitively.
(defun string-ci= (str1 str2)
  (declare (string str1 str2))
  (and (= (length str1) (length str2))
       (every #'char-ci= str1 str2)))

;; utils.lisp ends here
