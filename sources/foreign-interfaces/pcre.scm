(declare (unit pcre))

(foreign-declare "

#include <pcre.h>

// wraps the pcre_compile function
pcre* pcre_compile_wrapped(char* pattern)
{
  const char* error;
  int erroffset;

  pcre* pcre = pcre_compile(pattern, 0, &error, &erroffset, NULL);
  return pcre;
}

// wraps the pcre_study function
pcre_extra* pcre_study_wrapped(pcre* code)
{
  const char* error;

  pcre_extra* pcre_extra = pcre_study(code, 0, &error);
  if (pcre_extra == NULL)
  {
    if (error != NULL)
    {
      return NULL;
    }

    pcre_extra = calloc(1, sizeof(pcre_extra));
  }

  return pcre_extra;
}

// wraps the pcre_exec function
char** pcre_exec_wrapped(pcre* code, pcre_extra* extra, char* subject)
{
  char** result = NULL;

  int ovector[30];

  int pcre_exec_result = pcre_exec(code, extra, subject, strlen(subject), 0, 0, ovector, 30);
  if (pcre_exec_result < 0)
  {
    result = malloc(sizeof(char*));
    result[0] = NULL;
    return result;
  }

  result = malloc(sizeof(char*) * (pcre_exec_result + 1));

  int index = 0;
  for (index = 0; index < pcre_exec_result; index++)
  {
    pcre_get_substring(subject, ovector, pcre_exec_result, index, (const char**)&(result[index]));
  }

  result[pcre_exec_result] = NULL;
  return result;
}

")

;; pcre pointers definitions
(define-foreign-type pcre "pcre")
(define-foreign-type pcre* (c-pointer pcre))

;; pcre-extra pointers definitions
(define-foreign-type pcre-extra "pcre_extra")
(define-foreign-type pcre-extra* (c-pointer pcre-extra))

;; pcre-exec-result pointers definitions
(define-foreign-type pcre-exec-result "pcre_exec_result")
(define-foreign-type pcre-exec-result* (c-pointer pcre-exec-result))

;; compiles a regular expression into an internal form
(define pcre-compile (foreign-lambda pcre* "pcre_compile_wrapped" c-string))

;; studies a compiled pattern
(define pcre-study (foreign-lambda pcre-extra* "pcre_study_wrapped" pcre*))

;; matches a compiled regular expression against a given subject string
(define pcre-exec (foreign-lambda c-string-list* "pcre_exec_wrapped" pcre* pcre-extra* c-string))

;; frees a pcre
(define pcre-free (foreign-lambda void "pcre_free" pcre*))

;; frees a pcre study
(define pcre-free-study (foreign-lambda void "pcre_free_study" pcre-extra*))
