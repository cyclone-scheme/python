(import (scheme base)
        (scheme write)
        (cyclone test)
        (cyclone python)
        (srfi 69))

;; Needs:
;; $ pip install numpy

(py-start)
(py-import "numpy")

(py-def "print" exp)
(py-def "numpy.arange" n)

(define a (numpy.arange 15))
(display (%py-object-type a)) (newline)
(print a)
(define b (numpy.arange 12))
(display (%py-object-type b)) (newline)
(print b)

(py-stop)
