(import (scheme base)
        (scheme write)
        (cyclone test)
        (cyclone python)
        (srfi 69))

(py-start)

(py-import "numpy")

(py-define "print" exp)
(py-define "numpy.arange" n)
(py-define "numpy.reshape" n x y)
(py-define ("-" py-) a b)

(define a (numpy.arange 15))
(display (py-object-type a)) (newline)
(print a)
(define b (numpy.arange 12))
(display (py-object-type b)) (newline)
;; (print b)

(print (py-eval "print(a-b)"))

(py-stop)
