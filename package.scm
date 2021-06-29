(package
 (name python)
 (version "0.2.0")
 (license "BSD")
 (authors "Ivan Raikov" "Arthur Maciel")
 (maintainers "Arthur Maciel")
 (description "A Python FFI library")
 (tags "FFI")
 (docs "https://github.com/cyclone-scheme/cyclone-winds/wiki/python")
 (test "test.scm")
 (dependencies (pathname))
 (test-dependencies ())
 (foreign-dependencies ())
 (library
     (name (cyclone python))
   (description "Python FFI library")))
