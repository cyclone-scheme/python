(import (scheme base)
        (scheme write)
        (cyclone test)
        (cyclone python)
        (srfi 69))
(c-compiler-options "-g")

;; (test-begin)

(py-start)
(py-import "math")

(test-group "py-eval"
  (test 2 (python->scm (py-eval "1+1"))))

(test-group "py-value"
  (test 3.14159265358979 (py-value "math.pi")))

(test-group "py-define"
  (py-define "math.floor" n)
  (test 3 (math.floor 3.14159265358979)))

;; (test-group "py-define-attribute"

;;   )

;; (define l (py-list-new 2))
;; (py-list-set-item l 0 '(1 2 3))
;; (py-list-set-item l 1 '(4 5 6))

;; (define-pyfun "numpy.array")
(py-stop)
;; (test-end)


(test-exit)
