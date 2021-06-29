# python

## Index 
- [Intro](#Intro)
- [Dependencies](#Dependencies)
- [Test dependencies](#Test-dependencies)
- [Foreign dependencies](#Foreign-dependencies)
- [API](#API)
- [Examples](#Examples)
- [Author(s)](#Author(s))
- [Maintainer(s)](#Maintainer(s))
- [Version](#Version) 
- [License](#License) 
- [Tags](#Tags) 

## Intro 


## Dependencies 
None

## Test-dependencies 
None

## Foreign-dependencies 
None

## API 
The API provides a set of high-level procedures and another with low-level ones. Only high-level procedures are documented here.

### (cyclone python)

#### [procedure]   `(py-start)`
Mandatory to run it **before** any other procedure from this package. Run only once.

#### [procedure]   `(py-stop)`
Mandatory to run **after** any other procedure from this package to avoid memory leaking. Run only once.

#### [procedure]   `(py-run-simple-string code)`
This is the simplest way to run Python code. We can, for example, run:

```scheme
(py-run-simple-string "print(1+2)")
;; => 3
```

But note that this procedure does not return data. Use `py-value` or `py` bellow to get the return value in Scheme format.

#### [procedure]   `(py-run-file file)`
If we already have a Python project, we can use this procedure to run it. See the [run-file.scm](https://github.com/cyclone-scheme/python/blob/master/examples/run-file.scm) example.

#### [procedure]   `(py-value var)`
Evaluates `var` and returns it. See the [test file](https://github.com/cyclone-scheme/python/blob/master/test.scm) for example of usage.

#### [syntax]   `(py-def name [arg1 ...])`
This macro converts a Python procedure into a Scheme one. The `name` can be a string or a list (not quoted) with a string (the Python procedure name) and a symbol (the Scheme name for the procedure). `arg1` is optional. 

#### [syntax]   `(py-def-method name [arg1])`
This macro converts a Python object method into a Scheme procedure. 

The `name` can be a string or a list (not quoted) with a string (the Python method name) and a symbol (the Scheme name for the created procedure). `arg1` is optional. 

The generated procedure is called with the method's parent object as the first parameter and has a rest parameter, so other arguments can be passed.

See the [h5py.scm](https://github.com/cyclone-scheme/python/blob/master/examples/h5py.scm) and [swriter.scm](https://github.com/cyclone-scheme/python/blob/master/examples/swriter.scm) for examples.

#### [syntax]   `(py-def-attribute name)`
This macro converts a Python object attribute into a Scheme *procedure*. If you want to set the attribute into a variable, use `py-value` instead. 

The generated procedure is called with the attribute's parent object as the first parameter. If a second parameter is given, the attribute is *set* to it.

The `name` can be a string or a list (not quoted) containing a string (the Python attribute name) and a symbol (the Scheme name for the created procedure). `arg1` is optional. 

See the [h5py.scm](https://github.com/cyclone-scheme/python/blob/master/examples/h5py.scm) and [swriter.scm](https://github.com/cyclone-scheme/python/blob/master/examples/swriter.scm) for examples.

#### [procedure]   `(py-call module procedure-or-attribute [arg1 ...])`
This is the quickest form to retrieve Python values from module procedures or attributes. See the [test file](https://github.com/cyclone-scheme/python/blob/master/test.scm) for example of usage.

#### [syntax]   `(with-python command1 ...)`
A wrapper around the high-level commands above that runs `(py-start)` and `(py-stop)` automatically.

```Scheme
(with-python
 (py-import "math")
 (py-value "math.pi"))
```

is equivalent to
```Scheme
(py-start)
(py-import "math")
(py-value "math.pi")
(py-stop)
```

## Examples
See [the examples directories](https://github.com/cyclone-scheme/python/tree/master/examples).

## Author(s)
- Ivan Raikov
- Arthur Maciel

## Maintainer(s) 
Arthur Maciel

## Version 
"0.2.0"

## License 
BSD

## Tags 
FFI
