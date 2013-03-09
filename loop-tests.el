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

(ert-deftest loop-test-while-break ()
  (let ((x 0)
        (sum 0))
    ;; sum the numbers 0 to 5
    (loop-while (< x 10)
      (setq sum (+ sum x))
      (setq x (1+ x))
      (when (= x 6)
        (loop-break)))
    (should (equal sum 15))))

(ert-deftest loop-test-while-continue ()
  (let ((x 0)
        (sum 0))
    ;; sum the numbers 1, 3, 4, 5
    (loop-while (< x 5)
      (setq x (1+ x))
      (when (= x 2)
        (loop-continue))
      (setq sum (+ sum x)))
    (should (equal sum 13))))

(ert-deftest loop-test-do-while ()
  (let ((x 0)
        (sum 0))
    ;; our condition is false on the first iteration
    (loop-do-while (and (> x 0) (< x 10))
      (setq sum (+ sum x))
      (setq x (1+ x)))
    (should (equal sum 45))))

(ert-deftest loop-test-until ()
  (let ((x 0)
        (sum 0))
    ;; sum the numbers 0 to 9
    (loop-until (= x 10)
      (setq sum (+ sum x))
      (setq x (1+ x)))
    (should (equal sum 45))))

(ert-deftest loop-test-until-break ()
  (let ((x 0)
        (sum 0))
    ;; sum the numbers 0 to 5
    (loop-until (= x 10)
      (setq sum (+ sum x))
      (setq x (1+ x))
      (when (= x 6)
        (loop-break)))
    (should (equal sum 15))))

(ert-deftest loop-test-for-each ()
  (let ((sum 0))
    (loop-for-each x (list 1 2 3 4 5 6 7 8 9)
      (setq sum (+ sum x)))
    (should (equal sum 45))))

(ert-deftest loop-test-for-each-break ()
  (let ((sum 0))
    ;; sum the numbers 1 to 5
    (loop-for-each x (list 1 2 3 4 5 6 7 8 9)
      (setq sum (+ sum x))
      (when (= x 5)
        (loop-break)))
    (should (equal sum 15))))

(defun loop-run-tests ()
  (interactive)
  (ert-run-tests-interactively "loop-test-"))
