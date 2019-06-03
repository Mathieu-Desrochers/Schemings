define-configuration
--------------------
Defines a configuration.

    (define-configuration starship-configuration
      (fire-power integer #t 1 100)
      (torpedo-sizes list #t 3 5 integer #t 1 20)
      (mission starship-mission-subconfiguration #t))

    (define-configuration starship-mission-subconfiguration
      (objective string #t 1 500 default "Look around")
      (target-locations list #t 1 5 starship-target-location-subconfiguration #t))

    (define-configuration starship-target-location-subconfiguration
      (star-system string #t 1 500))

Supported field types (validation flags):

- boolean
- integer (required? min-value max-value)
- number (required? min-value max-value)
- string (required? min-length max-length)
- list (required? min-length max-length)
- subconfiguration (required?)

A default value can be specified for the following types:

- boolean
- integer
- number
- string

record definitions
------------------
Expands to these record definitions.

    (define-record starship
      fire-power
      torpedo-sizes
      mission)

    (define-record starship-mission-subconfiguration
      objective
      target-locations)

    (define-record starship-target-location-subconfiguration
      address)

validation procedure
--------------------
Expands to a validation procedure.  
Returns a list of errors.

    (define (validate-starship-configuration starship-configuration)
      ...)

parsing procedure
-----------------
Expands to a parsing procedure.  
Returns the parsed configuration.

    (define (parse-starship-configuration configuration-node)
      ...)

typed getters
-------------
Every field getter is defined with a * suffixed twin.  
These getters are annotated with their type, and  
should be used once validation has passed.

    (define (starship-configuration-fire-power* starship-configuration)
      ...)

try it
------
Place the following configuration in starship.cfg.

    fire-power = 100;
    torpedo-sizes = [1, 2, 8, 7];

    mission = {
      objective = "Milk run";
    };

Place the following code in sources/main.scm.

    ;; as above
    (define-configuration ...)
    (define-configuration ...)
    (define-configuration ...)

    (display
      (validate-starship-configuration
        (make-starship-configuration
          "Huge"
          (list 1 35 4 8)
          (make-starship-mission-subconfiguration
            "Milk run"
            (list
              (make-starship-target-location-subconfiguration "alpha zeta pox")
              (make-starship-target-location-subconfiguration #f))))))
    (newline)
    (newline)

    (read-configuration-file "starship.cfg"
      (lambda (configuration-node)
        (let ((configuration (parse-starship-configuration configuration-node)))
          (display
            (starship-configuration-fire-power configuration))
          (newline)
          (display
            (starship-configuration-torpedo-sizes configuration))
          (newline)
          (display
            (starship-mission-subconfiguration-objective
              (starship-configuration-mission configuration)))
          (newline))))

Run the following commands.

    $ make
    $ ./main

    (fire-power-wrong-type
      torpedo-sizes-1-too-high
      mission-target-locations-1-star-system-missing)

    100
    (1 2 8 7)
    Milk run
