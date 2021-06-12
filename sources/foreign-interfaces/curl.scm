(declare (unit curl))

(foreign-declare "

#include <curl/curl.h>

// structure for the received data
struct curl_write_data
{
  char* memory;
  size_t size;
};

// callback for the received data
size_t curl_easy_perform_write_callback(char* ptr, size_t size, size_t nmemb, void* userdata)
{
  struct curl_write_data* curl_write_data = (struct curl_write_data*)userdata;

  size_t realsize = size * nmemb;
  curl_write_data->memory = realloc(curl_write_data->memory, curl_write_data->size + realsize + 1);
  if (curl_write_data->memory == NULL)
  {
    return 0;
  }

  memcpy(&(curl_write_data->memory[curl_write_data->size]), ptr, realsize);
  curl_write_data->size += realsize;
  curl_write_data->memory[curl_write_data->size] = 0;

  return realsize;
}

// wraps the curl_easy_perform function
char* curl_easy_perform_wrapped(CURL* curl, int* result_code)
{
  struct curl_write_data* curl_write_data = malloc(sizeof(struct curl_write_data));
  curl_write_data->memory = calloc(1, 1);
  curl_write_data->size = 0;

  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, curl_easy_perform_write_callback);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, curl_write_data);

  *result_code = curl_easy_perform(curl);

  if (*result_code != CURLE_OK)
  {
    free(curl_write_data->memory);
    free(curl_write_data);
    return NULL;
  }

  char* result = curl_write_data->memory;
  free(curl_write_data);

  return result;
}

// allocates a curl_slist*
struct curl_slist** malloc_curl_slist_pointer()
{
  struct curl_slist** curl_slist_pointer = calloc(1, sizeof(struct curl_slist*));
  return curl_slist_pointer;
}

// adds a string to a curl_slist*
void curl_slist_append_wrapped(struct curl_slist** curl_slist_pointer, const char* string)
{
  *curl_slist_pointer = curl_slist_append(*curl_slist_pointer, string);
}

// frees a curl_slist*
void free_curl_slist_pointer(struct curl_slist** curl_slist_pointer)
{
  if (*curl_slist_pointer != NULL)
  {
    curl_slist_free_all(*curl_slist_pointer);
  }

  free(curl_slist_pointer);
}

// sets string options for a curl easy handle
long curl_easy_setopt_strings(CURL* curl, long option, struct curl_slist** curl_slist_pointer)
{
  return curl_easy_setopt(curl, option, *curl_slist_pointer);
}

")

;; constants
(define curle-ok (foreign-value "CURLE_OK" long))
(define curl-global-all (foreign-value "CURL_GLOBAL_ALL" long))
(define curlinfo-response-code (foreign-value "CURLINFO_RESPONSE_CODE" long))
(define curlopt-copypostfields (foreign-value "CURLOPT_COPYPOSTFIELDS" long))
(define curlopt-customrequest (foreign-value "CURLOPT_CUSTOMREQUEST" long))
(define curlopt-upload (foreign-value "CURLOPT_UPLOAD" long))
(define curlopt-url (foreign-value "CURLOPT_URL" long))
(define curlopt-userpwd (foreign-value "CURLOPT_USERPWD" long))
(define curlopt-httpheader (foreign-value "CURLOPT_HTTPHEADER" long))

;; curl pointers definitions
(define-foreign-type curl "CURL")
(define-foreign-type curl* (c-pointer curl))

;; global curl initialisation and cleanup
(define curl-global-init (foreign-lambda void "curl_global_init" long))
(define curl-global-cleanup (foreign-lambda void "curl_global_cleanup"))

;; curl-slist pointers definitions
(define-foreign-type curl-slist "struct curl_slist")
(define-foreign-type curl-slist* (c-pointer curl-slist))
(define-foreign-type curl-slist** (c-pointer curl-slist*))

;; curl-slist pointers memory management
(define malloc-curl-slist* (foreign-lambda curl-slist** "malloc_curl_slist_pointer"))
(define free-curl-slist* (foreign-lambda void "free_curl_slist_pointer" curl-slist**))

;; adds a string to an slist
(define curl-slist-append (foreign-lambda void "curl_slist_append_wrapped" curl-slist** (const c-string)))

;; starts and ends a libcurl easy session
(define curl-easy-init (foreign-lambda curl* "curl_easy_init"))
(define curl-easy-cleanup (foreign-lambda void "curl_easy_cleanup" curl*))

;; extract information from a curl handle
(define curl-easy-getinfo-long (foreign-lambda long "curl_easy_getinfo" curl* long s64vector))

;; sets options for a curl easy handle
(define curl-easy-setopt-string (foreign-lambda long "curl_easy_setopt" curl* long (const c-string)))
(define curl-easy-setopt-strings (foreign-lambda long "curl_easy_setopt_strings" curl* long curl-slist**))

;; url encodes the given string
(define curl-easy-escape (foreign-lambda c-string* "curl_easy_escape" curl* (const c-string) int))

;; performs a blocking file transfer
(define curl-easy-perform (foreign-lambda c-string* "curl_easy_perform_wrapped" curl* s32vector))
