(import (scheme base)
        (cyclone python))

(py-start)
(py-run-file "run-file.py")
(newline)
(py-run-file "run-file2.py")
(newline)
;; Needs matplotlib installed!
(py-run-file "run-file3.py")
(py-stop)
