;; Imported from https://github.com/iraikov/chicken-pyffi/blob/master/examples/h5py.scm
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

;; Needs:
;; $ pip install h5py

(py-start)
(py-import "h5py")

;; Define all procedures, methods and attributes
(py-def ("h5py.File" h5py-create-file) name mode)
(py-def-method ("close" close-file)) ;; File
(py-def-method ("create_dataset" file-create-dataset) name shape dtype) ;; File
(py-def-attribute ("name" dataset-name))
(py-def-attribute ("shape" dataset-shape))
(py-def-attribute ("dtype" dataset-dtype))

(define f (h5py-create-file "mytestfile.hdf5" "w"))

(define dset (file-create-dataset f "mydataset"  (vector 100) "i"))

(display (dataset-name dset)) (newline)
(display (dataset-shape dset)) (newline)
(display (dataset-dtype dset)) (newline)

(close-file f)
(py-stop)
