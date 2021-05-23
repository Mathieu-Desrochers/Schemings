(declare (unit time))

(foreign-declare "

#include <time.h>

// allocates a tm
struct tm* malloc_tm()
{
  struct tm* tm = malloc(sizeof(struct tm));

  if (tm != NULL)
  {
    tm->tm_sec = 0;
    tm->tm_min = 0;
    tm->tm_hour = 0;
    tm->tm_mday = 1;
    tm->tm_mon = 0;
    tm->tm_year = 0;
  }

  return tm;
}

// gets the values pointed by a tm*
int tm_sec(struct tm* tm) { return tm->tm_sec; }
int tm_min(struct tm* tm) { return tm->tm_min; }
int tm_hour(struct tm* tm) { return tm->tm_hour; }
int tm_mday(struct tm* tm) { return tm->tm_mday; }
int tm_wday(struct tm* tm) { return tm->tm_wday; }
int tm_mon(struct tm* tm) { return tm->tm_mon; }
int tm_year(struct tm* tm) { return tm->tm_year; }

// sets the values pointed by a tm*
void tm_sec_set(struct tm* tm, int value) { tm->tm_sec = value; }
void tm_min_set(struct tm* tm, int value) { tm->tm_min = value; }
void tm_hour_set(struct tm* tm, int value) { tm->tm_hour = value; }
void tm_mday_set(struct tm* tm, int value) { tm->tm_mday = value; }
void tm_mon_set(struct tm* tm, int value) { tm->tm_mon = value; }
void tm_year_set(struct tm* tm, int value) { tm->tm_year = value; }

// frees a tm
void free_tm(struct tm* tm)
{
  free(tm);
}

// wraps the gmtime_r function
struct tm* gmtime_r_wrapped(int64_t timer, struct tm* tm)
{
  return gmtime_r(&timer, tm);
}

// wraps the strftime function
char* strftime_wrapped(const char* format, const struct tm* tm)
{
  char* result = malloc(64 * sizeof(char));

  int strftime_result = strftime(result, 64, format, tm);
  if (strftime_result == 0)
  {
    free(result);
    return NULL;
  }

  return result;
}

")

;; tm pointers definitions
(define-foreign-type tm "struct tm")
(define-foreign-type tm* (c-pointer tm))

;; tm pointers memory management
(define malloc-tm (foreign-lambda tm* "malloc_tm"))
(define free-tm (foreign-lambda void "free_tm" tm*))

;; returns the number of seconds since the epoch
(define time* (foreign-lambda integer64 "time" (c-pointer integer64)))

;; breaks down the number of seconds since epoch
(define gmtime-r (foreign-lambda tm* "gmtime_r_wrapped" integer64 tm*))

;; gets the parts of a broken down time
(define tm-sec (foreign-lambda int "tm_sec" tm*))
(define tm-min (foreign-lambda int "tm_min" tm*))
(define tm-hour (foreign-lambda int "tm_hour" tm*))
(define tm-mday (foreign-lambda int "tm_mday" tm*))
(define tm-wday (foreign-lambda int "tm_wday" tm*))
(define tm-mon (foreign-lambda int "tm_mon" tm*))
(define tm-year (foreign-lambda int "tm_year" tm*))

;; sets the parts of a broken down time
(define tm-sec-set! (foreign-lambda void "tm_sec_set" tm* int))
(define tm-min-set! (foreign-lambda void "tm_min_set" tm* int))
(define tm-hour-set! (foreign-lambda void "tm_hour_set" tm* int))
(define tm-mday-set! (foreign-lambda void "tm_mday_set" tm* int))
(define tm-mon-set! (foreign-lambda void "tm_mon_set" tm* int))
(define tm-year-set! (foreign-lambda void "tm_year_set" tm* int))

;; recombines the number of seconds since epoch
(define timegm (foreign-lambda integer64 "timegm" tm*))

;; format a broken down time
(define strftime (foreign-lambda c-string* "strftime_wrapped" (const c-string) (const tm*)))

;; parses a broken down time
(define strptime (foreign-lambda c-string "strptime" (const c-string) (const c-string) tm*))
