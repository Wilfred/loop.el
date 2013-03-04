;;; loop.el --- friendly imperative loop structures

;; Copyright (C) 2013 Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 1.0
;; Keywords: loop, while, for each, break, continue

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs lisp is missing loop structures familiar to users of newer
;; languages. This library adds a selection of popular loop structures
;; as well as break and continue.

;;; Todo:

;; * Implement
;; * Document
;; * Examples
;; * Unit test
;; * Fix indentation

;; Things to implement:

;; loop-break, loop-continue, loop-return

(require 'cl) ;; gensym

(defmacro loop-while (condition &rest body)
  "Repeatedly evaluate BODY while CONDITION is non-nil."
  (declare (indent defun))
  `(catch 'loop-break
     (while ,condition ,@body)))

(defmacro loop-do-while (condition &rest body)
  "Evaluate BODY, then repeatedly BODY while CONDITION is non-nil."
  (declare (indent defun))
  (let ((is-first-iteration-var (gensym)))
    `(catch 'loop-break
       (progn
         ,@body
         (while ,condition
           ,@body)))))

(defmacro loop-until (condition &rest body)
  "Repeatedly evaluate BODY until CONDITION is non-nil."
  (declare (indent defun))
  `(catch 'loop-break
     (loop-while (not ,condition) ,@body)))

;; todo: support vectors and strings
(defmacro loop-for-each (var list &rest body)
  "For every item in LIST, evaluate BODY with VAR bound to that item."
  (declare (indent defun))
  (let ((list-var (gensym)))
    `(catch 'loop-break
       (let ((,list-var ,list)
              (,var))
          (while ,list-var
            (setq ,var (car ,list-var))
            (setq ,list-var (cdr ,list-var))
            ,@body)))))

(defun loop-break ()
  "Terminate evaluation of a loop-while, loop-do-while, or loop-for-each block.
If there are nested loops, breaks out of the innermost loop."
  (throw 'loop-break nil))

(provide 'loop)
;;; loop.el ends here
