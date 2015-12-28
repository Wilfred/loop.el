# loop.el --- friendly imperative loop structures for Emacs lisp

[![Build Status](https://travis-ci.org/Wilfred/loop.el.svg)](https://travis-ci.org/Wilfred/loop.el)
[![Coverage Status](https://coveralls.io/repos/Wilfred/loop.el/badge.svg)](https://coveralls.io/r/Wilfred/loop.el)
[![MELPA](http://melpa.org/packages/loop-badge.svg)](http://melpa.org/#/loop)
[![MELPA Stable](http://stable.melpa.org/packages/loop-badge.svg)](http://stable.melpa.org/#/loop)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

Emacs lisp is missing loop structures familiar to users of newer
languages. This library adds a selection of popular loop structures
as well as break and continue.

loop.el also has full unit tests.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [loop.el --- friendly imperative loop structures for Emacs lisp](#loopel-----friendly-imperative-loop-structures-for-emacs-lisp)
    - [Contents](#contents)
        - [while](#while)
        - [do while](#do-while)
        - [until](#until)
        - [for each](#for-each)
        - [break](#break)
        - [continue](#continue)
    - [Alternatives](#alternatives)
    - [Changelog](#changelog)
    - [Running the tests](#running-the-tests)

<!-- markdown-toc end -->

## Contents

### while

Repeatedly evaluate BODY while CONDITION is non-nil.

`loop-while` `(condition body...)`

Example:

``` lisp
(let ((x 0)
      (sum 0))
  ;; sum the numbers 0 to 9
  (loop-while (< x 10)
    (setq sum (+ sum x))
    (setq x (1+ x))))
```

### do while

Evaluate BODY, then repeatedly BODY while CONDITION is non-nil.

`loop-do-while` `(condition body...)`

Example:

``` lisp
(let ((x 0)
      (sum 0))
  ;; our condition is false on the first iteration
  (loop-do-while (and (> x 0) (< x 10))
    (setq sum (+ sum x))
    (setq x (1+ x))))
```

### until

Repeatedly evaluate BODY until CONDITION is non-nil.

`loop-until` `(condition body...)`

Example:

``` lisp
(let ((x 0)
      (sum 0))
  ;; sum the numbers 0 to 9
  (loop-until (= x 10)
    (setq sum (+ sum x))
    (setq x (1+ x))))
```

### for each

For every item in LIST, evaluate BODY with VAR bound to that item.

* `loop-for-each` `(var list body...)`

Example:

``` lisp
(let ((sum 0))
  (loop-for-each x (list 1 2 3 4 5 6 7 8 9)
    (setq sum (+ sum x))))
```

### break

Terminate evaluation of a `loop-while', `loop-do-while', or
`loop-for-each' block. If there are nested loops, breaks out of the
innermost loop.

`loop-break` `()`

Example:

``` lisp
(let ((sum 0))
  ;; sum the numbers 1 to 5
  (loop-for-each x (list 1 2 3 4 5 6 7 8 9)
    (setq sum (+ sum x))
    (when (= x 5)
      (loop-break))))
```

### continue

Skip the rest of the current `loop-while', `loop-do-while', or
`loop-for-each' block and continue to the next iteration. If there
are nested loops, applies to the innermost loop.

`loop-continue` `()`

Example:

``` lisp
(let ((sum 0))
  ;; sum the numbers 1, 3, 4, 5
  (loop-for-each x (list 1 2 3 4 5)
    (when (= x 2)
      (loop-continue))
    (setq sum (+ sum x))))
```

## Alternatives

* The traditional `while` loop
* The `loop` (more functional) and `dolist` (more imperative) macros in `cl`
* `-each` in [dash.el](https://github.com/magnars/dash.el)

## Changelog

* v1.1 Added `loop-continue`
* v1.0 `loop-for-each` now takes three arguments: `(VAR LIST BODY...)`
* v0.3 Added `loop-until`
* v0.2 Basic working implementation

## Running the tests

    M-x loop-run-tests
