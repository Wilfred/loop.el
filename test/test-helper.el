;;; test-helper --- Test helper for loop

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar loop-test-path
  (f-dirname (f-this-file)))

(defvar loop-root-path
  (f-parent loop-test-path))

(defvar loop-sandbox-path
  (f-expand "sandbox" loop-test-path))

(when (f-exists? loop-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" loop-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory loop-sandbox-path))
     (when (f-exists? loop-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir loop-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'ert)
(require 'el-mock)
(eval-when-compile
    (require 'cl))
(require 'undercover)
(undercover "loop.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))
(require 'loop)

(provide 'test-helper)
;;; test-helper.el ends here
