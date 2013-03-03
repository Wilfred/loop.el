# loop.el -- modern friendly loop structures for Emacs lisp

Emacs lisp is missing loop structures familiar to users of newer
languages. This library adds a selection of popular loop structures
as well as break and continue.

## Example usage

    (let ((x 0))
      ;; sets x to 6
      (loop-for-each (item (list 1 2 3))
         (setq x (+ x item))))

## Alternatives

* The traditional `while` and `dolist`

## Changelog

* v0.3 Added loop-until
* v0.2 Basic working implementation
