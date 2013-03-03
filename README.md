# loop.el -- modern friendly loop structures for Emacs lisp

Emacs lisp is missing loop structures familiar to users of newer
languages. This library adds a selection of popular loop structures
as well as break and continue.

## Contents

* `loop-while` `(condition body...)`
* `loop-do-while` `(condition body...)`
* `loop-until` `(condition body...)`
* `loop-for-each` `(var list body...)`

* `loop-break` `()`

## Example usage

    (let ((x 0))
      ;; sets x to 6
      (loop-for-each item (list 1 2 3)
         (setq x (+ x item))))

## Alternatives

* The traditional `while` and `dolist`
* The `loop` macro in `cl`

## Changelog

* v1.0 `loop-for-each` now takes three arguments: `(VAR LIST BODY...)`
* v0.3 Added `loop-until`
* v0.2 Basic working implementation
