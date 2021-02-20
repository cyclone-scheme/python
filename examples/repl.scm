(import (scheme base)
        (scheme write)
        (cyclone python)
        (scheme repl))
(c-compiler-options "-g")

(py-start)
(repl)
(py-stop)

