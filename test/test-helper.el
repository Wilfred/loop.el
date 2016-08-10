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

(require 'undercover)
(undercover "loop.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))

(provide 'test-helper)
;;; test-helper.el ends here
