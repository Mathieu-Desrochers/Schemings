(declare (unit statsd-client))

(foreign-declare "

#include <netinet/in.h>
#include <statsd/statsd-client.h>

")

;; statsd_link pointers definitions
(define-foreign-type statsd-link "statsd_link")
(define-foreign-type statsd-link* (c-pointer statsd-link))

;; initializes and finalizes statsd
(define statsd-init (foreign-lambda statsd-link* "statsd_init" (const c-string) int))
(define statsd-finalize (foreign-lambda void "statsd_finalize" statsd-link*))

;; sends statistics
(define statsd-inc (foreign-lambda int "statsd_inc" statsd-link* c-string float))
(define statsd-dec (foreign-lambda int "statsd_inc" statsd-link* c-string float))
(define statsd-count (foreign-lambda int "statsd_count" statsd-link* c-string int float))
(define statsd-gauge (foreign-lambda int "statsd_gauge" statsd-link* c-string int))
(define statsd-timing (foreign-lambda int "statsd_timing" statsd-link* c-string int))
