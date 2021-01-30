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

### (cyclone python)

#### [procedure]   `(raise-python-exception)`


#### [procedure]   `(python-exception-string)`


#### [variable]   `py-bool-as-bool`


#### [procedure]   `(scm->python value)`


#### [procedure]   `(python->scm value)`


#### [procedure]   `(py-object-type value)`


#### [procedure]   `(utf8-string->py-unicode value)`


#### [procedure]   `(py-unicode->utf8-string value)`


#### [variable]   `*py-functions*`


#### [procedure]   `(py-start)`


#### [procedure]   `(py-stop)`


#### [procedure]   `(py-eval expr)`


#### [procedure]   `(py-value var)`


#### [syntax]   `(py-define PARAMS)`


#### [syntax]   `(py-define-attribute PARAMS)`


#### [syntax]   `(py-define-method PARAMS)`


## Examples
```scheme
(import (scheme base)
        (cyclone python))
```

## Author(s)


## Maintainer(s) 


## Version 
0.1

## License 
BSD

## Tags 
