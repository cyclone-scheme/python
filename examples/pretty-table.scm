(import (scheme base)
        (scheme write)
        (cyclone python)
        (srfi 69))

(py-start)

;; Needs:
;; $ pip install prettytable

(py-import "prettytable")

(py-def "print" x)
(py-def "prettytable.PrettyTable")

(py-def-method "add_row" content)

(py-def-attribute "field_names")

(define x (prettytable.PrettyTable))
(field_names x '("City name" "Area" "Population" "Annual Rainfall"))
(add_row x '("Adelaide" "1295" "1158259" "600.5"))

(display (%py-object-type x)) (newline)

(print x)

(py-stop)
