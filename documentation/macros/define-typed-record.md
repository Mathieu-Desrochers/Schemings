define-typed-record
-------------------
Defines a typed record.

    (define-typed-record doughnut
      (name string)
      (calories fixnum)
      (ingredients (list-of string)))

Supported field types:  
See http://wiki.call-cc.org/man/4/Types

record definitions
------------------
Expands to this record definition.

    (: make-doughnut string fixnum (list-of string) -> (struct doughnut))

    (: doughnut-name ((struct doughnut) -> string))
    (: doughnut-calories ((struct doughnut) -> fixnum))
    (: doughnut-ingredients ((struct doughnut) -> (list-of string)))

    (: doughnut-name-set! ((struct doughnut) string -> noreturn))
    (: doughnut-calories-set! ((struct doughnut) fixnum -> noreturn))
    (: doughnut-ingredients-set! ((struct doughnut) (list-of string) -> noreturn))

    (define-record doughnut
      name
      calories
      ingredients)

make-record-copy
----------------
Makes a modified copy of a record.

    (make-record-copy
      (doughnut doughnut)
      (name "Double Double Chocolate")
      (calories 2000))

try it
------
Place the following code in sources/main.scm.

    ;; as above
    (define-typed-record ...)

    (let ((nice-doughnut (make-doughnut "Double Chocolate" 1000 (list "chocolate" "chocolate"))))
      (display (doughnut-name nice-doughnut))
      (newline)

      (let ((dream-doughnut
              (make-record-copy
                (doughnut nice-doughnut)
                (name "Double Double Chocolate")
                (calories 2000))))

        (display (doughnut-name dream-doughnut))
        (newline)))

Run the following commands.

    $ make
    $ ./main

    Double Chocolate
    Double Double Chocolate
