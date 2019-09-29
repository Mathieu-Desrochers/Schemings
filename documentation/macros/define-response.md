define-response
---------------
Defines a response.

    (define-response get-secret-base-response
      (name string)
      (secret-numbers list integer)
      (location get-secret-base-location-subresponse))

    (define-response get-secret-base-location-subresponse
      (address string)
      (capacity integer)
      (vehicules list get-secret-base-vehicule-subresponse))

    (define-response get-secret-base-vehicule-subresponse
      (color string))

Supported field types:

- blob
- boolean
- integer
- number
- string
- date
- date-time
- time
- list
- subresponse

record definitions
------------------
Expands to these record definitions.

    (define-record get-secret-base-response
      name
      secret-numbers
      location)

    (define-record get-secret-base-location-subresponse
      address
      capacity
      vehicules)

    (define-record get-secret-base-vehicule-subresponse
      color)

json formatting procedure
-------------------------
Expands to a json formatting procedure.  
Returns the formatted response.

    (define (get-secret-base-response->json-string get-secret-base-response)
      ...)

try it
------
Place the following code in sources/main.scm.

    (declare (uses json))

    ;; as above
    (define-response ...)
    (define-response ...)
    (define-response ...)

    (display
      (get-secret-base-response->json-string
        (make-get-secret-base-response
          "Fort Donuts"
          (list 1 4 8)
          (make-get-secret-base-location-subresponse
            "David's backyard"
            3
            (list
              (make-get-secret-base-vehicule-subresponse "red")
              (make-get-secret-base-vehicule-subresponse "blue"))))))
    (newline)))

Run the following commands.

    $ make
    $ ./main

    {
      "name": "Fort Donuts",
      "secret-numbers": [
        1,
        4,
        8
      ],
      "location": {
        "address": "David's backyard",
        "capacity": 3,
        "vehicules": [
          {
            "color": "red"
          },
          {
            "color": "blue"
          }
        ]
      }
    }
