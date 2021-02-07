;; https://docs.python.org/3/c-api/intro.html
;; https://github.com/iraikov/chicken-pyffi/blob/master/pyffi.scm








;; TODO - TODO - TODO
;; % to low-level procedures


(define-library (cyclone python)
  (import (scheme base)
          (only (scheme read) read-all)
          (only (scheme write) display)
          (only (scheme file) with-input-from-file)
          (only (scheme cyclone util) string-split)
          (only (srfi 1) filter take-while)
          (cyclone foreign)
          (srfi 69))
  (export ->string

          raise-python-exception
          python-exception-string

          opaque?
          opaque-null?
          
          py-initialize
          py-finalize

          py-incref
          py-decref
          
          py-object-call-object
          py-object-call
          py-object-get-attr-string
          py-object-set-attr-string
          py-object-str
          py-object-type-to-c-string

          py-bool-from-long
          py-bool-as-bool

          py-long-from-long
          py-long-as-long
          py-float-as-double
          py-float-from-double

          py-dict-new
          py-dict-keys
          py-dict-size
          py-dict-get-item
          py-dict-get-item-string
          py-dict-items
          py-dict-set-item

          py-list-new
          py-list-size
          py-list-get-item
          py-list-set-item

          py-tuple-new
          py-tuple-size
          py-tuple-get-item
          py-tuple-set-item

          py-object-get-buffer
          py-buffer-to-contiguous
          py-buffer-from-contiguous
          py-buffer-release
          py-buffer-size

          py-import-import-module-ex
          py-import-get-module-dict
          py-import-import
          py-import-import-module
          py-import-module-ex
          py-import-add-module
          py-module-get-dict
          py-module-get-dict-as-ptr
          py-module-add-object

          py-run-simple-string
          py-run-string
          py-run-file

          py-string-from-c-string
          py-string-to-c-string
          py-string-to-c-string-and-size
          py-unicode-from-c-string
          py-unicode-to-c-string-and-size
          string->py-unicode
          py-unicode->string

          ;; High-level
          py-start
          py-stop
          py-import
          py-eval
          py-apply
          py-value
          py-object-type
          scm->python
          python->scm
          
          *py-functions* 
          py-define
          py-define-attribute
          py-define-method)
  
  (include-c-header "<Python.h>")
  (c-linker-options "-lpython3.8")
  (c-compiler-options "-I/usr/include/python3.8")
  ;; (c-linker-options "-lpython3.9")
  ;; (c-compiler-options "-I/usr/include/python3.9")
  
  (begin
    (define-c opaque?
      "(void *data, int argc, closure _, object k, object p)"
      "return_closcall1(data, k, Cyc_is_opaque(p));")

    (define-c opaque-null?
      "(void *data, int argc, closure _, object k, object p)"
      "Cyc_check_opaque(data, p);
       return_closcall1(data, k, make_boolean(opaque_ptr(p) == NULL));")

;;; Foreign types
    (c-define-type py-object opaque scm->python python->scm)
    (c-define-type py-buffer opaque)
    (c-define-type py-ssize-t opaque)

;;; Procedures
    ;; Initialization and clean up
    (c-define py-initialize c-void "Py_Initialize")
    (c-define py-finalize c-void "Py_Finalize")

    ;; Reference counting
    (c-define py-incref c-void "Py_IncRef" opaque)
    (c-define py-decref c-void "Py_DecRef" opaque)

    ;; Error handling
    (c-define py-callable-check int "PyCallable_Check" opaque)
    (c-define py-err-occurred opaque "PyErr_Occurred")
    (c-define py-err-clear c-void "PyErr_Clear")

    (define-c py-err-as-string
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

    (define (raise-python-exception)
      (let ((msg (py-err-as-string)))
        (py-err-clear)
        (py-error msg)))

    (define (python-exception-string)
      (py-err-as-string))

    (define (py-error x . rest)
      (error (->string "Python: " x (unless (null? rest) rest))))

    ;; Python objects
    (c-define py-object-call-object py-object "PyObject_CallObject" opaque py-object)
    (c-define py-object-call py-object "PyObject_Call" opaque py-object py-object)
    (c-define py-object-get-attr-string py-object "PyObject_GetAttrString" opaque string)
    (c-define py-object-set-attr-string int "PyObject_SetAttrString" opaque string py-object)
    (c-define py-object-str py-object "PyObject_Str" opaque)

    (define-c py-object-type-to-c-string
      "(void *data, int argc, closure _, object k, object x)"
      "Cyc_check_opaque(data, x);

       PyObject *typ, *str;

       typ = PyObject_Type(opaque_ptr(x));
       str = PyObject_Str(typ);

       Py_DecRef(typ);

       make_utf8_string(data, res, PyUnicode_AsUTF8(str));
       
       return_closcall1(data, k, &res);")

    ;; Booleans
    (c-define py-bool-from-long opaque "PyBool_FromLong" int)
    (define-c py-bool->bool
      "(void *data, int argc, closure _, object k, object x)"
      "Cyc_check_opaque(data, x);
       return_closcall1 (data, k, make_boolean(opaque_ptr(x) == (Py_True)));")
    (define py-bool-as-bool py-bool->bool)

    ;; Numbers
    (c-define py-long-as-long int "PyLong_AsLong" opaque)
    (c-define py-long-from-long opaque "PyLong_FromLong" int)
    (c-define py-float-as-double double "PyFloat_AsDouble" opaque)
    (c-define py-float-from-double opaque "PyFloat_FromDouble" double)

    ;; Dicts
    (c-define py-dict-new opaque "PyDict_New")
    (c-define py-dict-keys py-object "PyDict_Keys" opaque)
    (c-define py-dict-size int "PyDict_Size" opaque)
    (c-define py-dict-get-item py-object "PyDict_GetItem" opaque py-object)
    (c-define py-dict-get-item-string py-object "PyDict_GetItemString" opaque string)
    (c-define py-dict-items py-object "PyDict_Items" opaque)
    (c-define py-dict-set-item int "PyDict_SetItem" opaque py-object py-object)

    ;; Lists
    (c-define py-list-new opaque "PyList_New" int)
    (c-define py-list-size int "PyList_Size" opaque)
    (c-define py-list-get-item py-object "PyList_GetItem" opaque int)
    (c-define py-list-set-item int "PyList_SetItem" opaque int py-object)

    ;; Tuples
    (c-define py-tuple-new opaque "PyTuple_New" int)
    (c-define py-tuple-size int "PyTuple_Size" opaque)
    (c-define py-tuple-get-item py-object "PyTuple_GetItem" opaque int)
    (c-define py-tuple-set-item int "PyTuple_SetItem" opaque int py-object)

    ;; Buffers
    (c-define py-object-get-buffer int "PyObject_GetBuffer" opaque py-buffer int)
    (c-define py-buffer-to-contiguous int "PyBuffer_ToContiguous" opaque py-buffer int char)
    (c-define py-buffer-from-contiguous int "PyBuffer_FromContiguous" py-buffer opaque int char)
    (c-define py-buffer-release c-void "PyBuffer_Release" py-buffer)

    (define-c py-buffer-size
      "(void *data, int argc, closure _, object k, object x)"
      "Cyc_check_opaque(data, x);

       return_closcall1 (data, k, obj_int2obj(((Py_buffer *)opaque_ptr(x))->len));")

    ;; Imports and modules
    (c-define py-import-import-module-ex opaque "PyImport_ImportModuleEx" string opaque opaque py-object)
    (c-define py-import-get-module-dict py-object "PyImport_GetModuleDict")
    (c-define py-import-import opaque "PyImport_Import" py-object)
    (c-define py-import-import-module opaque "PyImport_ImportModule" string)
    (c-define py-import-module-ex opaque "PyImport_ImportModuleEx" string opaque opaque opaque)
    (c-define py-import-add-module opaque "PyImport_AddModule" string)
    (c-define py-module-get-dict py-object "PyModule_GetDict" opaque)
    (c-define py-module-get-dict-as-ptr opaque "PyModule_GetDict" opaque)
    (c-define py-module-add-object int "PyModule_AddObject" opaque string opaque)

    ;; Evaluation
    (c-define py-run-simple-string c-void "PyRun_SimpleString" string)
    (c-define py-run-string opaque "PyRun_String" string int opaque opaque)

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

    (define py-run-file %py-run-file)

    ;; Strings
    (c-define py-string-from-c-string opaque "PyUnicode_FromString" string)
    (c-define py-string-to-c-string string "PyUnicode_AsUTF8" py-object)
    (c-define py-string-to-c-string-and-size string "PyUnicode_AsUTF8AndSize" py-object py-ssize-t)
    (define py-unicode-from-c-string py-string-from-c-string)
    (define py-unicode-to-c-string-and-size py-string-to-c-string-and-size)

;;; Misc util procedures
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
    
    (define (alist? x)  (and (list? x) (every pair? x)))

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

    (define-syntax translate-to-foreign
      (er-macro-transformer
       (lambda (x r c)
         (let ((%py-type-to (r 'py-type-to))
               (x (cadr x))
               (typ (caddr x)))
           `((,%py-type-to ,typ) ,x)))))

    (define-syntax translate-from-foreign
      (er-macro-transformer
       (lambda (x r c)
         (let ((%py-type-from (r 'py-type-from))
               (x (cadr x))
               (typ (caddr x)))
           `((,%py-type-from ,typ) ,x)))))

    (define (scm->python value)
      (cond
       ((exact-integer? value)  (translate-to-foreign value py-int))
       ((real? value)     (translate-to-foreign value py-float))
       ((alist? value)    (translate-to-foreign value py-dict))
       ((list? value)     (if (eq? 'ascii (car value)) 
                              (translate-to-foreign (cadr value) py-ascii)
                              (translate-to-foreign value py-list)))
       ((string? value)   (translate-to-foreign value py-unicode))
       ((vector? value)   (translate-to-foreign value py-tuple))
       ((bytevector? value)     (translate-to-foreign value py-buffer))
       ((boolean? value)  (translate-to-foreign value py-bool))
       ((opaque? value)  value)
       (else (py-error 'scm->python "invalid value " value))))

    (define (python->scm value)
      (if (not value) (raise-python-exception))
      (let ((typ-name  (py-object-type-to-c-string value)))
        (let ((typ-key (alist-ref typ-name *py-types* string=?)))
          (if typ-key
              (translate-from-foreign value typ-key)
              (begin
                (py-incref value)
                value)))))

    (define *py-types*
      `(("<class 'bool'>"      . ,py-bool)
        ("<class 'int'>"       . ,py-int)
        ("<class 'float'>"     . ,py-float)
        ("<class 'list'>"      . ,py-list)
        ("<class 'str'>"       . ,py-ascii)
        ("<class 'unicode'>"   . ,py-unicode)
        ("<class 'dict'>"      . ,py-dict)
        ("<class 'instance'>"  . ,py-instance)
        ("<class 'tuple'>"     . ,py-tuple)
        ("<class 'buffer'>"    . ,py-buffer)

        ("<type 'bool'>"      . ,py-bool)
        ("<type 'int'>"       . ,py-int)
        ("<type 'float'>"     . ,py-float)
        ("<type 'list'>"      . ,py-list)
        ("<type 'str'>"       . ,py-ascii)
        ("<type 'unicode'>"   . ,py-unicode)
        ("<type 'dict'>"      . ,py-dict)
        ("<type 'instance'>"  . ,py-instance)
        ("<type 'tuple'>"     . ,py-tuple)
        ("<type 'buffer'>"    . ,py-buffer)))
    
    (define (py-object-type value)
      (py-object-type-to-c-string value))
    
    (py-define-type py-int py-long-from-long py-long-as-long)

    (py-define-type py-tuple
                    (lambda (value)
                      (let* ((len (vector-length value))
                             (tup (py-tuple-new len)))
                        (if (opaque-null? tup) (raise-python-exception))
                        (let loop ((i 0))
                          (if (< i len) 
                              (begin
                                (unless (zero? (py-tuple-set-item tup i (vector-ref value i)))
                                  (raise-python-exception))
                                (loop (+ 1 i)))
                              tup))))
                    (lambda (value)
                      (let* ((len (py-tuple-size value))
                             (tup (make-vector len)))
                        (let loop ((i 0))
                          (if (< i len) 
                              (begin
                                (vector-set! tup i (py-tuple-get-item value i))
                                (loop (+ 1 i)))
                              tup)))))

    (py-define-type py-list
                    (lambda (value)
                      (let* ((len (length value))
                             (lst (py-list-new len)))
                        (if (opaque-null? lst) (raise-python-exception))
                        (let loop ((i 0))
                          (if (< i len)
                              (begin
                                (unless (zero? (py-list-set-item lst i (list-ref value i)))
                                  (raise-python-exception))
                                (loop (+ i 1)))
                              lst))))
                    (lambda (value)
                      (let ((len (py-list-size value)))
                        (let loop ((i 0) (lst (list)))
                          (if (< i len)
                              (let ((item (py-list-get-item value i)))
                                (loop (+ 1 i) (cons item lst)))
                              (begin
                                (reverse lst)))))))

    (py-define-type py-bool
                    (lambda (x) (py-bool-from-long (if x 1 0)))
                    py-bool-as-bool)

    (py-define-type py-float py-float-from-double py-float-as-double)

    (define (string->py-unicode value)
      ;; Given a Scheme UTF8 string, converts it into Python Unicode string
      (let ((res (py-unicode-from-c-string value)))
        res))

    (define (py-unicode->string value)
      ;; Given a Python Unicode string, converts it into Scheme UTF8 string
      (let ((res (py-string-to-c-string value)))
        res))

    (py-define-type py-ascii py-string-from-c-string py-string-to-c-string)
    (py-define-type py-unicode string->py-unicode py-unicode->string)

    (py-define-type py-dict 
                    ;; Given a Scheme alist, converts it into a Python dictionary
                    (lambda (value)
                      (let ((dct (py-dict-new)))
                        (if (not dct) (raise-python-exception))
                        (for-each
                         (lambda (kv)
                           (if (and (pair? kv) (pair? (cdr kv)))
                               (let ((k (car kv)) (v (cadr kv)))
                                 (if (not (zero? (py-dict-set-item dct k v)))
                                     (raise-python-exception)))
                               (py-error 'py-dict "invalid alist pair " kv)))
                         value)
                        dct))
                    ;; Given a Python dictionary, converts it into a Scheme alist
                    (lambda (value)
                      (let ((its (py-dict-items value)))
                        (let loop ((its its) (alst (list)))
                          (if (null? its) alst
                              (let ((item (car its)))
                                (let ((k (vector-ref item 0))
                                      (v (vector-ref item 1)))
                                  (loop (cdr its) (cons (list k v) alst)))))))))

    (py-define-type py-instance 
                    (lambda (x) x)
                    ;; Given a Python class instance, converts it into a Scheme alist
                    (lambda (value) (py-object-get-attr-string value "__dict__")))

    ;; TODO - adapt
    (py-define-type py-buffer 
                    ;; Given a Scheme blob, converts it into a Python buffer
                    (lambda (value)
                      (let ((buf (make-bytevector (bytevector-length value))))
                        (if (not buf) (raise-python-exception))
                        (py-buffer-from-contiguous buf value (bytevector-length value) #\C)
                        buf))
                    ;; Given a Python buffer, converts it into a Scheme blob
                    (lambda (value)
                      (let ((b (make-bytevector (py-buffer-size value))))
                        (py-buffer-to-contiguous b value (py-buffer-size value) #\C)
                        b)))

    (define +py-file-input+    257)
    (define +py-single-input+  256)
    (define +py-eval-input+    258)

    (define  *py-main-module* (make-parameter #f))
    (define  *py-main-module-dict* (make-parameter #f))
    (define  *py-functions* (make-hash-table eq? hash))

;;;; HIGH-LEVEL
    (define (py-start)
      (py-initialize)
      (*py-main-module* (py-import-add-module "__main__"))
      (*py-main-module-dict* (py-module-get-dict-as-ptr (*py-main-module*)))
      (py-incref (*py-main-module-dict*)))

    (define (py-stop)
      (when (*py-main-module-dict*)
        (py-decref (*py-main-module-dict*)))
      (*py-main-module* #f)
      (*py-main-module-dict* #f)
      (hash-table-clear! *py-functions*)
      (py-finalize))

    (define (py-import name . as)
      (let* ((as (if (null? as) #f (car as)))
             (p (string-split name "."))
             (id (if (null? p) name (car p))))
        (let ((m (py-import-import-module-ex name (*py-main-module-dict*) (py-dict-new) '())))
          (when (opaque-null? m)
            (raise-python-exception))
          (when (= -1 (py-module-add-object (*py-main-module*) id m))
            (py-decref m)
            (raise-python-exception))
          (if as
              (py-module-add-object (*py-main-module*) as m)
              (py-incref m)))))

    (define (py-eval expr)
      (let ((res (py-run-string expr
                                +py-eval-input+
                                (*py-main-module-dict*)
                                (py-dict-new))))
        (if (opaque-null? res)
            (raise-python-exception)
            res)))

    (define (py-apply func . rest)
      (py-object-call-object func (list->vector rest)))

    (define (py-value var)
      (let ((name (if (string? var)
                      var
                      (symbol->string var))))
        (python->scm (py-eval name))))
    
    (define-syntax py-define
      (er-macro-transformer
       (lambda (x r c)
         (let* ((%cons (r 'cons))
                (%define (r 'define))
                (%lambda (r 'lambda))
                (%let    (r 'let))
                (%begin  (r 'begin))
                (%set!   (r 'set!))
                (%unless (r 'unless))
                (%hash-table-ref/default (r 'hash-table-ref/default))
                (%hash-table-set! (r 'hash-table-set!))
                (py-eval   (r 'py-eval))
                (py-apply  (r 'py-apply))
                (raise-python-exception  (r 'raise-python-exception))
                (py-callable-check  (r 'py-callable-check))
                (py-decref         (r 'py-decref))
                (expr (cadr x))
                (args (cddr x))
                (func (r 'func))
                (proc-name (if (list? expr) 
                               (cadr expr) 
                               (string->symbol expr)))
                (form (if (list? expr) (car expr) expr)))
           `(,%define (,proc-name ,@args)
                      (,%let ((,func (,%hash-table-ref/default *py-functions* ',proc-name #f)))
                             (,%unless ,func
                                       (,%begin
                                        (,%set! ,func (,py-eval ,form))
                                        (,%unless ,func
                                                  (,raise-python-exception))
                                        ;; (,%unless (,py-callable-check ,func)
                                        ;;           (,py-decref ,func)
                                        ;;           (,raise-python-exception))
                                        (,%hash-table-set! *py-functions* ',proc-name ,func)))
                             (,py-apply ,func ,@args)))))))

    (define-syntax py-define-attribute
      (er-macro-transformer
       (lambda (x r c)
         (let ((name (cadr x)) 
               (rest (cddr x)))
           (let* ((scheme-name (if (null? rest) #f (car rest)))
                  (%define (r 'define))
                  (%if     (r 'if))
                  (%null?   (r 'null?))
                  (%car     (r 'car))
                  (py-object-get-attr-string     (r 'py-object-get-attr-string))
                  (py-object-set-attr-string     (r 'py-object-set-attr-string))
                  (proc-name   (or scheme-name (string->symbol name)))
                  (obj    (r 'obj))
                  (rest   (r 'rest)))
             `(,%define (,proc-name ,obj . ,rest)
                        (,%if (,%null? ,rest)
                              (,py-object-get-attr-string ,obj ,name)
                              (,py-object-set-attr-string ,obj ,name
                                                          (,%car ,rest)))))))))

    (define (identity x) x)

    (define-syntax py-define-method
      (er-macro-transformer
       (lambda (x r c)
         (let ((name (cadr x)) 
               (rest (cddr x)))
           (let ((scheme-name (member 'scheme-name rest))
                 (kw (member 'kw rest)))
             (let ((%define          (r 'define))
                   (%quote           (r 'quote))
                   (%cons            (r 'cons))
                   (%list            (r 'list))
                   (%identity        (r 'identity))
                   (%filter          (r 'filter))
                   (%take-while      (r 'take-while))
                   (%lambda          (r 'lambda))
                   (%symbol?         (r 'symbol?))
                   (%null?           (r 'null?))
                   (%and             (r 'and))
                   (%not             (r 'not))
                   (%if              (r 'if))
                   (%list->vector     (r 'list->vector))
                   (py-object-get-attr-string     (r 'py-object-get-attr-string))
                   (py-object-call-object        (r 'py-object-call-object))
                   (py-object-call              (r 'py-object-call))
                   (proc-name   (or (and scheme-name (cadr scheme-name)) (string->symbol name)))
                   (obj    (r 'obj))
                   (rest   (r 'rest)))
               `(,%define (,proc-name ,obj . ,rest)
                          (,py-object-call-object
                           (,py-object-get-attr-string ,obj ,name)
                           (,%list->vector ,rest)))

               ;; TODO: no keywords support ATM - need to extend Cyclone
               ;; (if (not kw)
               ;;     `(,%define (,proc-name ,obj . ,rest)
               ;;                (,py-object-call-object
               ;;                 (,py-object-get-attr-string ,obj ,(->string name) )
               ;;                 (,%list->vector ,rest)))
               ;;     (let ((kwargs (cadr kw)))
               ;;       `(,%define (,proc-name ,obj . ,rest #!key ,@(map (lambda (x) (list x #f)) kwargs))
               ;;                  (let ((kwargs (,%filter ,%identity 
               ;;                                          (,%list 
               ;;                                           ,@(map (lambda (k x) `(,%and ,x (,%list (,%->string (quote ,k)) ,x))) kwargs kwargs)))))
               ;;                    (,py-object-call 
               ;;                     (,py-object-get-attr-string ,obj ,(->string name) )
               ;;                     (,%list->vector (,%take-while (,%lambda (x) (,%not (,%symbol? x))) ,rest))
               ;;                     (,%if (,%null? kwargs) #f kwargs))))))
               ))))))))


;; Unused

;; (define-c py-unicode-ref
;;   "(void *data, int argc, closure _, object k, object x, object i)"
;;   "Cyc_check_opaque(data, x);
;;    Cyc_check_int(data, i);

;;    int *x_obj;
;;    int j = obj_obj2int(i);

;;    int result;

;;    if (j >= 0) {
;;      x_obj = opaque_ptr(x);
;;      result = x_obj[j];
;;    }
;;    else
;;      result = 0;

;;    return_closcall1 (data, k, obj_int2obj(result));")      

