;; Imported from https://github.com/iraikov/chicken-pyffi/blob/master/examples/h5py.scmp
;; Example from Python h5py documentation.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;

(import (scheme base)
        (scheme write)
        (cyclone python)
        (srfi 69))

(py-start)

(py-import "h5py")
(py-import "numpy")

;; Define all 
(py-define "h5py.File" name mode)
(py-define-method "close" scheme-name: close) ;; File
(py-define-method "create_dataset" name shape dtype) ;; File
(py-define-attribute "name" Dataset.name)
(py-define-attribute "shape" Dataset.shape)
(py-define-attribute "dtype" Dataset.dtype)

(define f (h5py.File "mytestfile.hdf5" "w"))

(define dset (create_dataset f "mydataset"  (vector 100) "i"))

(display (Dataset.name dset))
(newline)
(display (Dataset.shape dset))
(newline)
(display (Dataset.dtype dset))
(newline)

(close f)
(py-stop)
;; (py-start)
;; (py-import "prettytable")

;; (py-define "prettytable.PrettyTable")

;; (define x (prettytable.PrettyTable))

;; (py-define-attribute "field_names")

;; (field_names x '("City name" "Area" "Population" "Annual Rainfall"))


;; (py-py-define-method "add_row" content)

;; (add_row x '("Adelaide" "1295" "1158259" "600.5"))

;; (display (py-object-type x))
;; ;; (display (add_row x))
;; (py-stop)
