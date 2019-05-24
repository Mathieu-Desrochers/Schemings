(declare (unit icu))

(foreign-declare "

#include <unicode/ucasemap.h>
#include <unicode/ustring.h>
#include <unicode/utrans.h>

// allocates a UChar array
UChar* malloc_u_char(int size)
{
  UChar* array = calloc(size, sizeof(UChar));
  return array;
}

// frees a UChar array
void free_u_char(UChar* array)
{
  free(array);
}

// allocates a UErrorCode
UErrorCode* malloc_u_error_code()
{
  UErrorCode* errorCode = calloc(1, sizeof(UErrorCode));
  return errorCode;
}

// returns a UErrorCode value
UErrorCode u_error_code_value(UErrorCode* errorCode)
{
  return *errorCode;
}

// frees a UErrorCode
void free_u_error_code(UErrorCode* errorCode)
{
  free(errorCode);
}

")

;; u-char pointers definitions
(define-foreign-type u-char "UChar")
(define-foreign-type u-char* (c-pointer u-char))

;; u-char pointers memory management
(define malloc-u-char (foreign-lambda u-char* "malloc_u_char" int))
(define free-u-char (foreign-lambda void "free_u_char" u-char*))

;; u-case-map pointers definitions
(define-foreign-type u-case-map "UCaseMap")
(define-foreign-type u-case-map* (c-pointer u-case-map))

;; u-transliterator pointers definitions
(define-foreign-type u-transliterator "UTransliterator")
(define-foreign-type u-transliterator* (c-pointer u-transliterator))

;; u-error-code pointers definitions
(define-foreign-type u-error-code "UErrorCode")
(define-foreign-type u-error-code* (c-pointer u-error-code))

;; u-error-code pointers memory management
(define malloc-u-error-code (foreign-lambda u-error-code* "malloc_u_error_code"))
(define free-u-error-code (foreign-lambda void "free_u_error_code" u-error-code*))

;; returns the length of an array of u-char
(define u-strlen (foreign-lambda int "u_strlen" u-char*))

;; converts a UTF-8 string to UTF-16
(define u-str-from-utf8
  (foreign-lambda u-char* "u_strFromUTF8"
    u-char* int u32vector c-string int u-error-code*))

;; converts a UTF-16 string to UTF-8
(define u-str-to-utf8
  (foreign-lambda u-char* "u_strToUTF8"
    u8vector int u32vector u-char* int u-error-code*))

;; opens and closes a u-case-map
(define u-case-map-open (foreign-lambda u-case-map* "ucasemap_open" c-string int u-error-code*))
(define u-case-map-close (foreign-lambda void "ucasemap_close" u-case-map*))

;; upper cases the characters in a UTF-8 string
(define u-case-map-utf8-to-upper
  (foreign-lambda int "ucasemap_utf8ToUpper"
    u-case-map* u8vector int c-string int u-error-code*))

;; lower cases the characters in a UTF-8 string
(define u-case-map-utf8-to-lower
  (foreign-lambda int "ucasemap_utf8ToLower"
    u-case-map* u8vector int c-string int u-error-code*))

;; opens a u-transliterator
(define u-trans-open-u
  (foreign-lambda u-transliterator* "utrans_openU"
    u-char* int int u-char* int c-pointer u-error-code*))

;; transliterates a segment of a string
(define u-trans-trans-u-char
  (foreign-lambda void "utrans_transUChars"
    u-transliterator* u-char* u32vector int int u32vector u-error-code*))

;; closes a u-transliterator
(define u-trans-close (foreign-lambda void "utrans_close" u-transliterator*))

;; returns a u-error-code value
(define u-error-code-value (foreign-lambda int "u_error_code_value" u-error-code*))
