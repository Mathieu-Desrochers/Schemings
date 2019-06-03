define-typed-record
-------------------
Defines a typed record.

    (define-typed-record pasta-recipe
      (name string)
      (calories fixnum)
      (ingredients (list-of string))
      (tasty boolean))

Supported field types:  
See http://wiki.call-cc.org/man/4/Types

record definitions
------------------
Expands to this record definition.

    (: make-pasta-recipe string fixnum (list-of string) boolean -> (struct pasta-recipe))

    (: pasta-recipe-name ((struct pasta-recipe) -> string))
    (: pasta-recipe-calories ((struct pasta-recipe) -> fixnum))
    (: pasta-recipe-ingredients ((struct pasta-recipe) -> (list-of string)))
    (: pasta-recipe-tasty ((struct pasta-recipe) -> boolean))

    (: pasta-recipe-name-set! ((struct pasta-recipe) string -> noreturn))
    (: pasta-recipe-calories-set! ((struct pasta-recipe) fixnum -> noreturn))
    (: pasta-recipe-ingredients-set! ((struct pasta-recipe) (list-of string) -> noreturn))
    (: pasta-recipe-tasty-set! ((struct pasta-recipe) boolean -> noreturn))

    (define-record pasta-recipe
      name
      calories
      ingredients
      tasty)

try it
------
Place the following code in sources/main.scm.

    ;; as above
    (define-typed-record ...)

    (make-pasta-recipe "Eazy Noodles" 300 (list "pasta" 2 "milk") #t)

Run the following commands.

    $ make

    Warning: at toplevel:
      (sources/main.scm:7) in procedure call to `make-pasta-recipe',
      expected argument #3 of type `(list-of string)' but was given an
      argument of type `(list string fixnum string)'
