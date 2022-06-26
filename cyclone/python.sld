;; https://docs.python.org/3/c-api/intro.html
;; https://github.com/iraikov/chicken-pyffi/blob/master/pyffi.scm
(define-library (cyclone python)
  (import (scheme base)
          (scheme eval)
          (only (scheme write) display)
          (only (scheme cyclone util) string-split)
          (cyclone foreign)
          (cyclone pathname)
          (srfi 69))
  (include-c-header "<Python.h>")
  (c-compiler-options "`python3-config --includes`")
  (c-linker-options "`python3-config --includes | awk  '{print $1}' | sed 's/^.*\\//-l/'`")

  (export
   ;; High-level
   with-python
   py-start
   py-stop

   py-import

   py-run-simple-string
   py-run-file
   py-value
   py-def
   py-def-attribute
   py-def-method
   py-call

   ;; Misc
   ->string
   
   ;; Low-level
   %py-is-initialized
   %initialized?
   %py-initialize
   %py-finalize
   %with-initialization-check

   %py-incref
   %py-decref

   %py-callable-check
   %py-err-occurred
   %py-err-clear
   %py-err-as-string
   %raise-python-exception
   %python-exception-string
   %py-error

   %py-object-call-object
   %py-object-call
   %py-object-get-attr-string
   %py-object-set-attr-string
   %py-object-str
   %py-object-type

   %long->py-bool
   %py-bool->bool

   %long->py-long
   %py-long->long
   %py-float->double
   %py-float-from-double

   %string->py-str
   %py-str->string
   %py-str->string-and-size

   %py-dict-new
   %py-dict-keys
   %py-dict-size
   %py-dict-get-item
   %py-dict-get-item-string
   %py-dict-items
   %py-dict-set-item

   %py-list-new
   %py-list-size
   %py-list-get-item
   %py-list-set-item

   %py-tuple-new
   %py-tuple-size
   %py-tuple-get-item
   %py-tuple-set-item

   %py-object-get-buffer
   %py-buffer->contiguous
   %contiguous->py-buffer
   %py-buffer-release
   %py-buffer-size

   %py-import
   %py-import-module
   %py-import-module-ex
   %py-import-get-module-dict

   %py-import-add-module
   %py-module-get-dict
   %py-module-get-dict-as-ptr
   %py-module-add-object

   %py-run-simple-string
   %py-run-string
   %py-run-file
   %py-eval
   %py-apply
   *py-functions*

   %scm->python
   %python->scm)
  
  (begin
    (define-c opaque?
      "(void *data, int argc, closure _, object k, object p)"
      "return_closcall1(data, k, Cyc_is_opaque(p));")

    (define-c opaque-null?
      "(void *data, int argc, closure _, object k, object p)"
      "Cyc_check_opaque(data, p);
       return_closcall1(data, k, make_boolean(opaque_ptr(p) == NULL));")

;;; Foreign types
    (c-define-type py-object opaque %scm->python %python->scm)
    (c-define-type py-buffer opaque)
    (c-define-type py-ssize-t opaque)

;;; Procedures
    ;; Initialization and clean up
    (c-define %py-is-initialized int "Py_IsInitialized")
    (define (%initialized?) (not (= 0 (%py-is-initialized))))
    (c-define %py-initialize c-void "Py_Initialize")
    (c-define %py-finalize c-void "Py_Finalize")

    (define-syntax %with-initialization-check
      (syntax-rules ()
        ((_ rest ...)
         (if (%initialized?)
             (begin rest ...)
             (%py-error "Please run (py-start) first")))))
    
    ;; Reference counting
    (c-define %py-incref c-void "Py_IncRef" opaque)
    (c-define %py-decref c-void "Py_DecRef" opaque)

    ;; Error handling
    (c-define %py-callable-check int "PyCallable_Check" opaque)
    (c-define %py-err-occurred opaque "PyErr_Occurred")
    (c-define %py-err-clear c-void "PyErr_Clear")

    (define-c %py-err-as-string
      "(void *data, int argc, closure _, object k)"
      "PyObject *exc;

       PyGILState_STATE state = PyGILState_Ensure(); 

       exc = PyErr_Occurred();

       if (exc != NULL) {
           PyObject *str = PyObject_Str(exc);
           make_utf8_string(data, res, PyUnicode_AsUTF8(str));
           return_closcall1(data, k, &res);       
       }

       PyGILState_Release(state); 
       
       make_utf8_string(data, null_str, \"\");
       return_closcall1(data, k, &null_str);")

    (define (%raise-python-exception)
      (let ((msg (%py-err-as-string)))
        (%py-err-clear)
        (%py-error msg)))

    (define (%python-exception-string)
      (%py-err-as-string))

    (define (%py-error x . rest)
      (error (->string "Python: " x (unless (null? rest) rest))))

    ;; Python objects
    (c-define %py-object-call-object py-object "PyObject_CallObject" opaque py-object)
    (c-define %py-object-call py-object "PyObject_Call" opaque py-object py-object)
    (c-define %py-object-get-attr-string py-object "PyObject_GetAttrString" opaque string)
    (c-define %py-object-set-attr-string int "PyObject_SetAttrString" opaque string py-object)
    (c-define %py-object-str py-object "PyObject_Str" opaque)

    (define-c %py-object-type
      "(void *data, int argc, closure _, object k, object x)"
      "Cyc_check_opaque(data, x);

       PyObject *typ, *str;

       typ = PyObject_Type(opaque_ptr(x));
       str = PyObject_Str(typ);

       Py_DecRef(typ);

       make_utf8_string(data, res, PyUnicode_AsUTF8(str));
       
       return_closcall1(data, k, &res);")

    ;; Booleans
    (c-define %long->py-bool opaque "PyBool_FromLong" int)
    (define-c %py-bool->bool
      "(void *data, int argc, closure _, object k, object x)"
      "Cyc_check_opaque(data, x);
       return_closcall1 (data, k, make_boolean(opaque_ptr(x) == (Py_True)));")

    ;; Numbers
    (c-define %py-long->long int "PyLong_AsLong" opaque)
    (c-define %long->py-long opaque "PyLong_FromLong" int)
    (c-define %py-float->double double "PyFloat_AsDouble" opaque)
    (c-define %py-float-from-double opaque "PyFloat_FromDouble" double)

    ;; Strings
    (c-define %string->py-str opaque "PyUnicode_FromString" string)
    (c-define %py-str->string string "PyUnicode_AsUTF8" py-object)
    (c-define %py-str->string-and-size string "PyUnicode_AsUTF8AndSize" py-object py-ssize-t)

    ;; Dicts
    (c-define %py-dict-new opaque "PyDict_New")
    (c-define %py-dict-keys py-object "PyDict_Keys" opaque)
    (c-define %py-dict-size int "PyDict_Size" opaque)
    (c-define %py-dict-get-item py-object "PyDict_GetItem" opaque py-object)
    (c-define %py-dict-get-item-string py-object "PyDict_GetItemString" opaque string)
    (c-define %py-dict-items py-object "PyDict_Items" opaque)
    (c-define %py-dict-set-item int "PyDict_SetItem" opaque py-object py-object)

    ;; Lists
    (c-define %py-list-new opaque "PyList_New" int)
    (c-define %py-list-size int "PyList_Size" opaque)
    (c-define %py-list-get-item py-object "PyList_GetItem" opaque int)
    (c-define %py-list-set-item int "PyList_SetItem" opaque int py-object)

    ;; Tuples
    (c-define %py-tuple-new opaque "PyTuple_New" int)
    (c-define %py-tuple-size int "PyTuple_Size" opaque)
    (c-define %py-tuple-get-item py-object "PyTuple_GetItem" opaque int)
    (c-define %py-tuple-set-item int "PyTuple_SetItem" opaque int py-object)

    ;; Buffers
    (c-define %py-object-get-buffer int "PyObject_GetBuffer" opaque py-buffer int)
    (c-define %py-buffer->contiguous int "PyBuffer_ToContiguous" opaque py-buffer int char)
    (c-define %contiguous->py-buffer int "PyBuffer_FromContiguous" py-buffer opaque int char)
    (c-define %py-buffer-release c-void "PyBuffer_Release" py-buffer)

    (define-c %py-buffer-size
      "(void *data, int argc, closure _, object k, object x)"
      "Cyc_check_opaque(data, x);

       return_closcall1 (data, k, obj_int2obj(((Py_buffer *)opaque_ptr(x))->len));")

    ;; Imports and modules
    (c-define %py-import opaque "PyImport_Import" py-object)
    (c-define %py-import-module opaque "PyImport_ImportModule" string)
    (c-define %py-import-module-ex opaque "PyImport_ImportModuleEx" string opaque opaque py-object)
    (c-define %py-import-get-module-dict py-object "PyImport_GetModuleDict")
    (c-define %py-import-add-module opaque "PyImport_AddModule" string)
    (c-define %py-module-get-dict py-object "PyModule_GetDict" opaque)
    (c-define %py-module-get-dict-as-ptr opaque "PyModule_GetDict" opaque)
    (c-define %py-module-add-object int "PyModule_AddObject" opaque string opaque)

    ;; Evaluation
    (c-define %py-run-simple-string int "PyRun_SimpleString" string)
    (c-define %py-run-string opaque "PyRun_String" string int opaque opaque)

    (define-c %py-run-file
      "(void *data, int argc, closure _, object k, object f)"
      "Cyc_check_str(data, f);
       
       FILE *fp = fopen(string_str(f), \"r\");
       
       if (fp == NULL) 
         Cyc_rt_raise2(data, \"Could not open file\", f);

       PyObject *res = PyRun_File(fp, string_str(f), Py_file_input, PyDict_New(), NULL);

       fclose(fp);
       make_c_opaque(r, res);

       return_closcall1(data, k, &r);")

    (define (%py-eval expr)
      (let ((res (%py-run-string expr
                                 +py-eval-input+
                                 (*py-main-module-dict*)
                                 (%py-dict-new))))
        (if (opaque-null? res)
            (%raise-python-exception)
            res)))

    (define (%py-apply func . rest)
      (let ((res (%py-object-call-object func (list->vector rest))))
        (if (and (opaque? res) (opaque-null? res))
            (%raise-python-exception)
            res)))

;;; Misc util procedures
    (define (identity x) x)

    (define (->string obj . rest)
      (let ((str (open-output-string)))
        (let loop ((objs (cons obj rest)))
          (if (null? objs)
              (get-output-string str)
              (begin (display (car objs) str)
                     (display " " str)
                     (loop (cdr objs)))))))
    
    (define (hash-table-clear! ht)
      (map (lambda (k)
             (hash-table-delete! ht k))
           (hash-table-keys ht)))
    
    (define (alist? x) (and (list? x) (every pair? x)))

    (define (alist-ref elt alst comp)
      (let ((res (assoc elt alst)))
        (and res (cdr res))))

;;; Internal Scheme <-> Python translation
    (define-record-type py-type
      (make-py-type name to from)
      py-type?
      (name py-type-name py-type-name!)
      (to py-type-to py-type-to!)
      (from py-type-from py-type-from!))

    (define-syntax py-define-type
      (er-macro-transformer
       (lambda (x r c)
         (let ((%define (r 'define))
               (%make-py-type (r 'make-py-type))
               (name (cadr x))
               (to   (caddr x))
               (from (cadddr x)))
           `(,%define ,name (,%make-py-type ',name ,to ,from))))))

    (define-syntax %translate-to-foreign
      (er-macro-transformer
       (lambda (x r c)
         (let ((%py-type-to (r 'py-type-to))
               (x (cadr x))
               (typ (caddr x)))
           `((,%py-type-to ,typ) ,x)))))

    (define-syntax %translate-from-foreign
      (er-macro-transformer
       (lambda (x r c)
         (let ((%py-type-from (r 'py-type-from))
               (x (cadr x))
               (typ (caddr x)))
           `((,%py-type-from ,typ) ,x)))))

    (py-define-type py-int %long->py-long %py-long->long)

    (py-define-type py-tuple
                    (lambda (value)
                      (let* ((len (vector-length value))
                             (tup (%py-tuple-new len)))
                        (if (opaque-null? tup) (%raise-python-exception))
                        (let loop ((i 0))
                          (if (< i len) 
                              (begin
                                (unless (zero? (%py-tuple-set-item tup i (vector-ref value i)))
                                  (%raise-python-exception))
                                (loop (+ 1 i)))
                              tup))))
                    (lambda (value)
                      (let* ((len (%py-tuple-size value))
                             (tup (make-vector len)))
                        (let loop ((i 0))
                          (if (< i len) 
                              (begin
                                (vector-set! tup i (%py-tuple-get-item value i))
                                (loop (+ 1 i)))
                              tup)))))

    (py-define-type py-list
                    (lambda (value)
                      (let* ((len (length value))
                             (lst (%py-list-new len)))
                        (if (opaque-null? lst) (%raise-python-exception))
                        (let loop ((i 0))
                          (if (< i len)
                              (begin
                                (unless (zero? (%py-list-set-item lst i (list-ref value i)))
                                  (%raise-python-exception))
                                (loop (+ i 1)))
                              lst))))
                    (lambda (value)
                      (let ((len (%py-list-size value)))
                        (let loop ((i 0) (lst (list)))
                          (if (< i len)
                              (let ((item (%py-list-get-item value i)))
                                (loop (+ 1 i) (cons item lst)))
                              (begin
                                (reverse lst)))))))

    (py-define-type py-bool
                    (lambda (x) (%long->py-bool (if x 1 0)))
                    %py-bool->bool)

    (py-define-type py-float %py-float-from-double %py-float->double)

    (py-define-type py-str %string->py-str %py-str->string)

    (py-define-type py-dict 
                    ;; Given a Scheme alist, converts it into a Python dictionary
                    (lambda (value)
                      (let ((dct (%py-dict-new)))
                        (if (not dct) (%raise-python-exception))
                        (for-each
                         (lambda (kv)
                           (if (and (pair? kv) (pair? (cdr kv)))
                               (let ((k (car kv)) (v (cadr kv)))
                                 (if (not (zero? (%py-dict-set-item dct k v)))
                                     (%raise-python-exception)))
                               (%py-error 'py-dict "invalid alist pair " kv)))
                         value)
                        dct))
                    ;; Given a Python dictionary, converts it into a Scheme alist
                    (lambda (value)
                      (let ((its (%py-dict-items value)))
                        (let loop ((its its) (alst (list)))
                          (if (null? its) alst
                              (let ((item (car its)))
                                (let ((k (vector-ref item 0))
                                      (v (vector-ref item 1)))
                                  (loop (cdr its) (cons (list k v) alst)))))))))

    (py-define-type py-instance 
                    (lambda (x) x)
                    ;; Python class instance -> Scheme alist
                    (lambda (value) (%py-object-get-attr-string value "__dict__")))

    ;; TODO - adapt
    (py-define-type py-buffer 
                    ;; Scheme blob -> Python buffer
                    (lambda (value)
                      (let ((buf (make-bytevector (bytevector-length value))))
                        (if (not buf) (%raise-python-exception))
                        (%contiguous->py-buffer buf value (bytevector-length value) #\C)
                        buf))
                    ;; Python buffer -> Scheme blob
                    (lambda (value)
                      (let ((b (make-bytevector (%py-buffer-size value))))
                        (%py-buffer->contiguous b value (%py-buffer-size value) #\C)
                        b)))

    (define *py-types*
      `(("<class 'bool'>"      . ,py-bool)
        ("<class 'int'>"       . ,py-int)
        ("<class 'float'>"     . ,py-float)
        ("<class 'list'>"      . ,py-list)
        ("<class 'str'>"       . ,py-str)
        ("<class 'dict'>"      . ,py-dict)
        ("<class 'instance'>"  . ,py-instance)
        ("<class 'tuple'>"     . ,py-tuple)
        ("<class 'buffer'>"    . ,py-buffer)))
    
    (define (%scm->python value)
      (cond
       ((exact-integer? value) (%translate-to-foreign value py-int))
       ((real? value)          (%translate-to-foreign value py-float))
       ((alist? value)         (%translate-to-foreign value py-dict))
       ((list? value)          (%translate-to-foreign value py-list))
       ((string? value)        (%translate-to-foreign value py-str))
       ((vector? value)        (%translate-to-foreign value py-tuple))
       ((bytevector? value)    (%translate-to-foreign value py-buffer))
       ((boolean? value)       (%translate-to-foreign value py-bool))
       ((opaque? value)        value)
       (else (%py-error '%scm->python "invalid value " value))))
    
    (define (%python->scm value)
      (if (not value) (%raise-python-exception))
      (let ((typ-name  (%py-object-type value)))
        (let ((typ-key (alist-ref typ-name *py-types* string=?)))
          (if typ-key
              (%translate-from-foreign value typ-key)
              (begin
                (%py-incref value)
                value)))))

    (define +py-file-input+    257)
    (define +py-single-input+  256)
    (define +py-eval-input+    258)

    (define  *py-main-module* (make-parameter #f))
    (define  *py-main-module-dict* (make-parameter #f))
    (define  *py-functions* (make-hash-table eq? hash))

;;; High-level
    (define-syntax with-python
      (syntax-rules ()
        ((_ rest ...)
         (begin
           (py-start)
           (let ((result
                  (begin rest ...)))
             (py-stop)
             result)))))
    
    (define (py-start)
      (%py-initialize)
      (*py-main-module* (%py-import-add-module "__main__"))
      (*py-main-module-dict* (%py-module-get-dict-as-ptr (*py-main-module*)))
      (%py-incref (*py-main-module-dict*)))

    (define (py-stop)
      (when (*py-main-module-dict*)
        (%py-decref (*py-main-module-dict*)))
      (*py-main-module* #f)
      (*py-main-module-dict* #f)
      (hash-table-clear! *py-functions*)
      (%py-finalize))

    (define (py-import name . as)
      (%with-initialization-check
       (let* ((as (if (null? as) #f (car as)))
              (p (string-split name "."))
              (id (if (null? p) name (car p))))
         (let ((m (%py-import-module-ex name (*py-main-module-dict*) (%py-dict-new) '())))
           (when (opaque-null? m)
             (%raise-python-exception))
           (when (= -1 (%py-module-add-object (*py-main-module*) id m))
             (%py-decref m)
             (%raise-python-exception))
           (if as
               (when (= -1 (%py-module-add-object (*py-main-module*) as m))
                 (%py-decref m)
                 (%raise-python-exception))
               (%py-incref m))))))

    (define (py-run-simple-string code)
      (%with-initialization-check
       (when (= -1 (%py-run-simple-string code))
         (%raise-python-exception))))

    (define (py-run-file file)
      (%with-initialization-check
       (when (opaque-null? (%py-run-file file))
         (%raise-python-exception))))
    
    (define (py-value var)
      (%with-initialization-check
       (let ((name (if (string? var)
                       var
                       (symbol->string var))))
         (%python->scm (%py-eval name)))))

    
    ;; Functional helper macros
    ;; Accepted signatures for py-def, py-def-method and py-def-attr:
    ;; (py-def "math.floor" ...) ;; string
    ;; (py-def ("math.floor" floor) ...) ;; list of string + scheme name as symbol
    (define-syntax py-def
      (er-macro-transformer
       (lambda (x r c)
         (let* ((%define                  (r 'define))
                (%let                     (r 'let))
                (%set!                    (r 'set!))
                (%unless                  (r 'unless))
                (%when                    (r 'when))
                (%hash-table-ref/default  (r 'hash-table-ref/default))
                (%hash-table-set!         (r 'hash-table-set!))
                (py-eval                  (r '%py-eval))
                (py-apply                 (r '%py-apply))
                (raise-python-exception   (r '%raise-python-exception))
                (py-callable-check        (r '%py-callable-check))
                (py-decref                (r '%py-decref))
                (func                     (r 'func))
                (name                     (cadr x))
                (args                     (cddr x))
                (form                     (if (list? name) (car name) name))
                (proc-name                (if (list? name)
                                              (cadr name)
                                              (string->symbol name))))
           `(,%define (,proc-name ,@args)
                      (,%let ((,func (,%hash-table-ref/default *py-functions* ',proc-name #f)))
                             (,%unless ,func
                                       (,%set! ,func (,py-eval ,form))
                                       (,%unless ,func
                                                 (,raise-python-exception))
                                       (,%when (= 0 (,py-callable-check ,func))
                                               (,py-decref ,func)
                                               (,raise-python-exception))
                                       (,%hash-table-set! *py-functions* ',proc-name ,func))
                             (,py-apply ,func ,@args)))))))

    (define-syntax py-def-method
      (er-macro-transformer
       (lambda (x r c)
         (let* ((%define                    (r 'define))
                (%list->vector              (r 'list->vector))
                (py-object-get-attr-string  (r '%py-object-get-attr-string))
                (py-object-call-object      (r '%py-object-call-object))
                (obj                        (r 'obj))
                (rest                       (r 'rest))
                (name                       (cadr x))
                (method                     (if (list? name) (car name) name))
                (proc-name                  (if (list? name)
                                                (cadr name)
                                                (string->symbol name))))
           `(,%define (,proc-name ,obj . ,rest)
                      (,py-object-call-object
                       (,py-object-get-attr-string ,obj ,method)
                       (,%list->vector ,rest)))))))
    
    (define-syntax py-def-attribute
      (er-macro-transformer
       (lambda (x r c)
         (let* ((%define                    (r 'define))
                (%if                        (r 'if))
                (%null?                     (r 'null?))
                (%car                       (r 'car))
                (py-object-get-attr-string  (r '%py-object-get-attr-string))
                (py-object-set-attr-string  (r '%py-object-set-attr-string))
                (obj                        (r 'obj))
                (rest                       (r 'rest))                
                (name                       (cadr x))
                (attr                       (if (list? name) (car name) name))
                (proc-name                  (if (list? name)
                                                (cadr name)
                                                (string->symbol name))))
           `(,%define (,proc-name ,obj . ,rest)
                      (,%if (,%null? ,rest)
                            (,py-object-get-attr-string ,obj ,attr)
                            (,py-object-set-attr-string ,obj ,attr
                                                        (,%car ,rest))))))))
    
    (define (py-call obj meth-or-attr . args)
      (%with-initialization-check
       (let ((params (if (null? args) #f args))
             (form (%py-eval (string-append (->string obj) "." (->string meth-or-attr)))))
         (if params
             (apply %py-apply (cons form params))
             (%python->scm form)))))))

;;; For future development
;; From: https://stackoverflow.com/questions/3210238/how-do-i-get-list-of-all-python-types-programmatically
;; >>> import builtins
;; >>> builtin_types = [getattr(builtins, d) for d in dir(builtins) if isinstance(getattr(builtins, d), type)]
;; >>> from pprint import pprint 
;; >>> pprint(builtin_types)
;; # Skipped the errors and warning types
;;         <class 'Exception'>,
;;         <class 'GeneratorExit'>,
;;         <class 'KeyboardInterrupt'>,
;;         <class 'StopAsyncIteration'>,
;;         <class 'StopIteration'>,
;;         <class 'SystemExit'>,
;;         <class 'Warning'>,
;;         <class 'bool'>,
;;         <class 'bytearray'>,
;;         <class 'bytes'>,
;;         <class 'classmethod'>,
;;         <class 'complex'>,
;;         <class 'dict'>,
;;         <class 'enumerate'>,
;;         <class 'filter'>,
;;         <class 'float'>,
;;         <class 'frozenset'>,
;;         <class 'int'>,
;;         <class 'list'>,
;;         <class 'map'>,
;;         <class 'memoryview'>,
;;         <class 'object'>,
;;         <class 'property'>,
;;         <class 'range'>,
;;         <class 'reversed'>,
;;         <class 'set'>,
;;         <class 'slice'>,
;;         <class 'staticmethod'>,
;;         <class 'str'>,
;;         <class 'super'>,
;;         <class 'tuple'>,
;;         <class 'type'>,
;;         <class 'zip'>]
