define-request
--------------
Defines a request.

    (define-request new-secret-base-request
      (name string #t 1 50)
      (secret-numbers list #t 3 5 integer #t 1 20)
      (location new-secret-base-location-subrequest #t))

    (define-request new-secret-base-location-subrequest
      (address string #t 1 50)
      (capacity integer #t 1 1000)
      (vehicules list #t 1 10 new-secret-base-vehicule-subrequest #t))

    (define-request new-secret-base-vehicule-subrequest
      (color string #t 1 50))

Supported field types (validation flags):

- blob (required?)
- boolean
- integer (required? min-value max-value)
- number (required? min-value max-value)
- string (required? min-length max-length)
- date (required?)
- date-time (required?)
- time (required?)
- list (required? min-length max-length)
- subrequest (required?)

record definitions
------------------
Expands to these record definitions.

    (define-record new-secret-base-request
      name
      secret-numbers
      location)

    (define-record new-secret-base-location-subrequest
      address
      capacity
      vehicules)

    (define-record new-secret-base-vehicule-subrequest
      color)

validation procedure
--------------------
Expands to a validation procedure.  
Returns a list of errors.

    (define (validate-new-secret-base-request new-secret-base-request)
      ...)

json parsing procedure
----------------------
Expands to a json parsing procedure.  
Returns the parsed request.

    (define (json-parse-new-secret-base-request json-node)
      ...)

typed getters
-------------
Every field getter is defined with a * suffixed twin.  
These getters are annotated with their type, and  
should be used once validation has passed.

    (define (new-secret-base-request-name* new-secret-base-request)
      ...)

try it
------
Place the following code in sources/main.scm.

    (declare (uses json))

    (define-request new-secret-base-request
      (name string #t 1 50)
      (secret-numbers list #t 3 5 integer #t 1 20)
      (location new-secret-base-location-subrequest #t))

    (define-request new-secret-base-location-subrequest
      (address string #t 1 50)
      (capacity integer #t 1 1000)
      (vehicules list #t 1 10 new-secret-base-vehicule-subrequest #t))

    (define-request new-secret-base-vehicule-subrequest
      (color string #t 1 50))

    (display
      (validate-new-secret-base-request
        (make-new-secret-base-request
          "Fort Donuts"
          (list 1 35 4 8)
          (make-new-secret-base-location-subrequest
            "David's backyard"
            "Three persons"
            (list
              (make-new-secret-base-vehicule-subrequest "red")
              (make-new-secret-base-vehicule-subrequest #f))))))
    (newline)
    (newline)

    (with-string->json
      (string-append
        "{"
        "  \"name\": \"Fort Donuts\", "
        "  \"secret-numbers\": [1, 35, 4, 8], "
        "  \"location\": {"
        "    \"address\": \"David's backyard\""
        "  }"
        "}")
      (lambda (json-node)
        (let ((request (json-parse-new-secret-base-request json-node)))
          (display
            (new-secret-base-request-name request))
          (newline)
          (display
            (new-secret-base-request-secret-numbers request))
          (newline)
          (display
            (new-secret-base-location-subrequest-address
              (new-secret-base-request-location request)))
          (newline))))

Run the following commands.

    $ make
    $ ./main

    (secret-numbers-1-too-high
      location-capacity-wrong-type
      location-vehicules-1-color-missing)

    Fort Donuts
    (1 35 4 8)
    David's backyard
