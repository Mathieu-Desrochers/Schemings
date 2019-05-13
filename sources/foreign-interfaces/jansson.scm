(declare (unit jansson))

(foreign-declare "

#include <jansson.h>

")

;; jansson types definition
(define-foreign-type json-t "json_t")
(define-foreign-type json-t* (c-pointer json-t))
(define-foreign-type json-error-t "json_error_t")
(define-foreign-type json-error-t* (c-pointer json-error-t))

;; returns a new jansson object
(define json-object (foreign-lambda json-t* "json_object"))

;; returns a new jansson array
(define json-array (foreign-lambda json-t* "json_array"))

;; returns a new jansson value
(define json-boolean (foreign-lambda json-t* "json_boolean" int))
(define json-integer (foreign-lambda json-t* "json_integer" int))
(define json-real (foreign-lambda json-t* "json_real" double))
(define json-string (foreign-lambda json-t* "json_string" c-string))
(define json-null (foreign-lambda json-t* "json_null"))

;; sets the value of key to value in object
(define json-object-set-new (foreign-lambda int "json_object_set_new" json-t* c-string json-t*))

;; appends a value to the end of array
(define json-array-append-new (foreign-lambda int "json_array_append_new" json-t* json-t*))

;; returns the jansson representation as a string
(define json-indent (foreign-value "JSON_INDENT(2)" int))
(define json-preserve-order (foreign-value "JSON_PRESERVE_ORDER" int))
(define json-dumps (foreign-lambda c-string* "json_dumps" json-t* int))

;; decodes the jansson string input
(define json-reject-duplicates (foreign-value "JSON_REJECT_DUPLICATES" int))
(define json-loads (foreign-lambda json-t* "json_loads" c-string unsigned-integer json-error-t*))

;; returns the type of the jansson value
(define json-type-object (foreign-value "JSON_OBJECT" int))
(define json-type-array (foreign-value "JSON_ARRAY" int))
(define json-type-string (foreign-value "JSON_STRING" int))
(define json-type-integer (foreign-value "JSON_INTEGER" int))
(define json-type-real (foreign-value "JSON_REAL" int))
(define json-type-true (foreign-value "JSON_TRUE" int))
(define json-type-false (foreign-value "JSON_FALSE" int))
(define json-type-null (foreign-value "JSON_NULL" int))
(define json-typeof (foreign-lambda int "json_typeof" json-t*))

;; returns the associated value
(define json-integer-value (foreign-lambda int "json_integer_value" json-t*))
(define json-real-value (foreign-lambda double "json_real_value" json-t*))
(define json-string-value (foreign-lambda c-string "json_string_value" json-t*))

;; gets a value corresponding to key from object
(define json-object-get (foreign-lambda json-t* "json_object_get" json-t* c-string))

;; returns the number of elements in array
(define json-array-size (foreign-lambda int "json_array_size" json-t*))

;; returns the element in array at position index
(define json-array-get (foreign-lambda json-t* "json_array_get" json-t* int))

;; decrements the reference count
(define json-decref (foreign-lambda void "json_decref" json-t*))
