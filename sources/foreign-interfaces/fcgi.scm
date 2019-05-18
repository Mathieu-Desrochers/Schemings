(declare (unit fcgi))

(foreign-declare "

#include <fcgi_config.h>
#include <fcgi_stdio.h>

// allocates a FCGX_Stream*
FCGX_Stream** malloc_fcgx_stream_pointer()
{
  FCGX_Stream** fcgx_stream_pointer = malloc(sizeof(FCGX_Stream*));
  return fcgx_stream_pointer;
}

// resolves a FCGX_Stream*
FCGX_Stream* resolve_fcgx_stream_pointer(FCGX_Stream** fcgx_stream_pointer)
{
  return *fcgx_stream_pointer;
}

// frees a FCGX_Stream*
void free_fcgx_stream_pointer(FCGX_Stream** fcgx_stream_pointer)
{
  free(fcgx_stream_pointer);
}

// allocates a FCGX_ParamArray
FCGX_ParamArray* malloc_fcgx_paramarray()
{
  FCGX_ParamArray* fcgx_paramarray = malloc(sizeof(FCGX_ParamArray));
  return fcgx_paramarray;
}

// resolves a FCGX_ParamArray
FCGX_ParamArray resolve_fcgx_paramarray(FCGX_ParamArray* fcgx_paramarray)
{
  return *fcgx_paramarray;
}

// frees a FCGX_ParamArray
void free_fcgx_paramarray(FCGX_ParamArray* fcgx_paramarray)
{
  free(fcgx_paramarray);
}

// wraps the FCGX_PutStr function
int FCGX_PutStr_wrapped(const unsigned char* str, int n, FCGX_Stream* stream)
{
  return FCGX_PutStr((char*)str, n, stream);
}

// wraps the FCGX_GetStr function
int FCGX_GetStr_wrapped(unsigned char* str, int n, FCGX_Stream* stream)
{
  return FCGX_GetStr((char*)str, n, stream);
}

")

;; fcgx-stream pointers definitions
(define-foreign-type fcgx-stream "FCGX_Stream")
(define-foreign-type fcgx-stream* (c-pointer fcgx-stream))
(define-foreign-type fcgx-stream** (c-pointer fcgx-stream*))

;; fcgx-stream pointers memory management
(define malloc-fcgx-stream* (foreign-lambda fcgx-stream** "malloc_fcgx_stream_pointer"))
(define resolve-fcgx-stream* (foreign-lambda fcgx-stream* "resolve_fcgx_stream_pointer" fcgx-stream**))
(define free-fcgx-stream* (foreign-lambda void "free_fcgx_stream_pointer" fcgx-stream**))

;; fcgx-paramarray pointers definitions
(define-foreign-type fcgx-paramarray (c-pointer (c-pointer char)))
(define-foreign-type fcgx-paramarray* (c-pointer fcgx-paramarray))

;; fcgx-paramarray pointers memory management
(define malloc-fcgx-paramarray (foreign-lambda fcgx-paramarray* "malloc_fcgx_paramarray"))
(define resolve-fcgx-paramarray (foreign-lambda fcgx-paramarray "resolve_fcgx_paramarray" fcgx-paramarray*))
(define free-fcgx-paramarray (foreign-lambda void "free_fcgx_paramarray" fcgx-paramarray*))

;; accepts a new request from the http server
(define fcgx-accept
  (foreign-lambda int "FCGX_Accept"
    fcgx-stream** fcgx-stream** fcgx-stream** fcgx-paramarray*))

;; obtains value of fcgi parameter in environment
(define fcgx-getparam (foreign-lambda c-string "FCGX_GetParam" (const c-string) fcgx-paramarray))

;; reads up to n consecutive bytes from the input stream
(define fcgx-getstr (foreign-lambda int "FCGX_GetStr_wrapped" u8vector int fcgx-stream*))

;; writes a null-terminated character string to the output stream
(define fcgx-puts (foreign-lambda int "FCGX_PutS" (const c-string) fcgx-stream*))

;; writes consecutive bytes to the output stream
(define fcgx-putstr (foreign-lambda int "FCGX_PutStr_wrapped" (const blob) int fcgx-stream*))

;; finishes the current request from the http server
(define fcgx-finish (foreign-lambda void "FCGX_Finish"))
