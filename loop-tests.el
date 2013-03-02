(require 'ert)
(require 'loop)

(ert-deftest loop-test-while ()
  (let ((x 0)
        (sum 0))
    ;; sum the numbers 0 to 9
    (loop-while (< x 10)
      (setq sum (+ sum x))
      (setq x (1+ x)))
    (should (equal sum 45))))

(defun loop-run-tests ()
  (interactive)
  (ert-run-tests-interactively "loop-test-"))
