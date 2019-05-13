(import (chicken condition))
(import (chicken format))

(declare (unit json))

(declare (uses exceptions))
(declare (uses jansson))
(declare (uses json-intern))

;; encapsulates a json node
(define-typed-record json-node
  (json-t* pointer))

;; invokes a procedure with a new object node
(: with-json-object (forall (r) (((struct json-node) -> r) -> r)))
(define (with-json-object procedure)
  (with-guaranteed-release
    make-object-json-t*
    (lambda (json-t*)
      (procedure (make-json-node json-t*)))
    (lambda (json-t*)
      (json-decref json-t*))))

;; invokes a procedure with a new array node
(: with-json-array (forall (r) (((struct json-node) -> r) -> r)))
(define (with-json-array procedure)
  (with-guaranteed-release
    make-array-json-t*
    (lambda (json-t*)
      (procedure (make-json-node json-t*)))
    (lambda (json-t*)
      (json-decref json-t*))))

;; adds a value node to an object node
(: json-object-add-value ((struct json-node) string * -> noreturn))
(define (json-object-add-value json-node property-name property-value)
  (let* ((json-t* (json-node-json-t* json-node))
         (property-json-t* (make-value-json-t* property-value))
         (json-object-set-new-result (json-object-set-new json-t* property-name property-json-t*)))
    (unless (eq? json-object-set-new-result 0)
      (json-decref property-json-t*)
      (abort
        (format "failed to add value node for property ~A and value ~A"
          property-name
          property-value)))))

;; adds an object node to an object node
(: json-object-add-object ((struct json-node) string -> (struct json-node)))
(define (json-object-add-object json-node property-name)
  (let* ((json-t* (json-node-json-t* json-node))
         (property-json-t* (make-object-json-t*))
         (json-object-set-new-result (json-object-set-new json-t* property-name property-json-t*)))
    (unless (eq? json-object-set-new-result 0)
      (free-json-node property-json-node)
      (abort
        (format "failed to add object node for property ~A"
          property-name)))
    (make-json-node property-json-t*)))

;; adds an array node to an object node
(: json-object-add-array ((struct json-node) string -> (struct json-node)))
(define (json-object-add-array json-node property-name)
  (let* ((json-t* (json-node-json-t* json-node))
         (property-json-t* (make-array-json-t*))
         (json-object-set-new-result (json-object-set-new json-t* property-name property-json-t*)))
    (unless (eq? json-object-set-new-result 0)
      (free-json-node property-json-node)
      (abort
        (format "failed to add array node for property ~A"
          property-name)))
    (make-json-node property-json-t*)))

;; adds a value node to an array node
(: json-array-add-value ((struct json-node) * -> noreturn))
(define (json-array-add-value json-node value)
  (let* ((json-t* (json-node-json-t* json-node))
         (property-json-t* (make-value-json-t* value))
         (json-array-append-new-result (json-array-append-new json-t* property-json-t*)))
    (unless (eq? json-array-append-new-result 0)
      (json-decref property-json-t*)
      (abort
        (format "failed to add value node for value ~A"
          value)))))

;; adds an object node to an array node
(: json-array-add-object ((struct json-node) -> (struct json-node)))
(define (json-array-add-object json-node)
  (let* ((json-t* (json-node-json-t* json-node))
         (element-json-t* (make-object-json-t*))
         (json-array-append-new-result (json-array-append-new json-t* element-json-t*)))
    (unless (eq? json-array-append-new-result 0)
      (free-json-node property-json-node)
      (abort "failed to add object node"))
    (make-json-node element-json-t*)))

;; adds an array node to an array node
(: json-array-add-array ((struct json-node) -> (struct json-node)))
(define (json-array-add-array json-node)
  (let* ((json-t* (json-node-json-t* json-node))
         (element-json-t* (make-array-json-t*))
         (json-array-append-new-result (json-array-append-new json-t* element-json-t*)))
    (unless (eq? json-array-append-new-result 0)
      (free-json-node property-json-node)
      (abort "failed to add array node"))
    (make-json-node element-json-t*)))

;; formats a json node to string
(: json->string ((struct json-node) -> string))
(define (json->string json-node)
  (json-dumps
    (json-node-json-t* json-node)
    (+ json-indent json-preserve-order)))

;; invokes a procedure with a parsed json node
(: with-string->json (forall (r) (string ((or (struct json-node) false) -> r) -> r)))
(define (with-string->json string procedure)
  (with-guaranteed-release
    (lambda ()
      (json-loads string json-reject-duplicates #f))
    (lambda (json-t*)
      (if json-t*
        (procedure (make-json-node json-t*))
        (procedure #f)))
    (lambda (json-t*)
      (when json-t* (json-decref json-t*)))))

;; returns a value from an object node
(: json-object-get-value ((struct json-node) string -> *))
(define (json-object-get-value json-node property-name)
  (let* ((json-t* (json-node-json-t* json-node))
         (property-json-t* (json-object-get json-t* property-name)))
    (if property-json-t*
      (get-value-from-json-t* property-json-t*)
      #f)))

;; returns an object node from an object node
(: json-object-get-object ((struct json-node) string -> (or (struct json-node) false)))
(define (json-object-get-object json-node property-name)
  (let* ((json-t* (json-node-json-t* json-node))
         (property-json-t* (json-object-get json-t* property-name)))
    (if property-json-t*
      (let ((json-type (json-typeof property-json-t*)))
        (if (eq? json-type json-type-object)
          (make-json-node property-json-t*)
          #f))
      #f)))

;; returns an array node from an object node
(: json-object-get-array ((struct json-node) string -> (or (struct json-node) false)))
(define (json-object-get-array json-node property-name)
  (let* ((json-t* (json-node-json-t* json-node))
         (property-json-t* (json-object-get json-t* property-name)))
    (if property-json-t*
      (let ((json-type (json-typeof property-json-t*)))
        (if (eq? json-type json-type-array)
          (make-json-node property-json-t*)
          #f))
      #f)))

;; returns a value from an array node
(: json-array-get-value ((struct json-node) fixnum -> *))
(define (json-array-get-value json-node index)
  (let* ((json-t* (json-node-json-t* json-node))
         (element-json-t* (json-array-get json-t* index)))
    (if element-json-t*
      (get-value-from-json-t* element-json-t*)
      #f)))

;; returns an object node from an array node
(: json-array-get-object ((struct json-node) fixnum -> (or (struct json-node) false)))
(define (json-array-get-object json-node index)
  (let* ((json-t* (json-node-json-t* json-node))
         (element-json-t* (json-array-get json-t* index)))
    (if element-json-t*
      (let ((json-type (json-typeof element-json-t*)))
        (if (eq? json-type json-type-object)
          (make-json-node element-json-t*)
          #f))
      #f)))

;; returns an array node from an array node
(: json-array-get-array ((struct json-node) fixnum -> (or (struct json-node) false)))
(define (json-array-get-array json-node index)
  (let* ((json-t* (json-node-json-t* json-node))
         (element-json-t* (json-array-get json-t* index)))
    (if element-json-t*
      (let ((json-type (json-typeof element-json-t*)))
        (if (eq? json-type json-type-array)
          (make-json-node element-json-t*)
          #f))
      #f)))

;; returns the length of an array node
(: json-array-length ((struct json-node) -> fixnum))
(define (json-array-length json-node)
  (let ((json-t* (json-node-json-t* json-node)))
    (json-array-size json-t*)))
