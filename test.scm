(import (scheme base)
        (scheme write)
        (cyclone test)
        (cyclone python)
        (srfi 69))

(py-start)
(py-import "math")

(test-group "%py-eval"
  (test 2 (python->scm (%py-eval "1+1"))))

(test-group "py-value"
  (test 3.14159265358979 (py-value "math.pi")))

(test-group "py-call"
  (test 3.14159265358979 (py-call "math" "pi")))

(test-group "py-def"
  (py-def "math.floor" n)
  (test 3 (math.floor 3.14159265358979)))

(py-stop)

