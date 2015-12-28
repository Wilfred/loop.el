# loop.el --- friendly imperative loop structures for Emacs lisp

[![Build Status](https://travis-ci.org/Wilfred/loop.el.svg)](https://travis-ci.org/Wilfred/loop.el)
[![Coverage Status](https://coveralls.io/repos/Wilfred/loop.el/badge.svg)](https://coveralls.io/r/Wilfred/loop.el)
[![MELPA](http://melpa.org/packages/loop-badge.svg)](http://melpa.org/#/loop)
[![MELPA Stable](http://stable.melpa.org/packages/loop-badge.svg)](http://stable.melpa.org/#/loop)
[![Tag Version](https://img.shields.io/github/tag/Wilfred/loop.el.svg)](https://github.com/Wilfred/loop.el/tags)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

Emacs lisp is missing loop structures familiar to users of newer
languages. This library adds a selection of popular loop structures
as well as break and continue.

loop.el also has full unit tests.

## Contents

* `loop-while` `(condition body...)`
* `loop-do-while` `(condition body...)`
* `loop-until` `(condition body...)`
* `loop-for-each` `(var list body...)`

* `loop-break` `()`
* `loop-continue` `()`

## Example usage

    (let ((x 0))
      ;; sets x to 6
      (loop-for-each item (list 1 2 3)
         (setq x (+ x item))))

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
