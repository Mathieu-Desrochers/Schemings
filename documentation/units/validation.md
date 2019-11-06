validate-blob
-------------
Validates a blob value.

__value__  
A value to validate.

__result__  
Returns #f if no errors were found, or one of the following symbols.

- 'missing
- 'wrong-type
- 'invalid-value

validate-boolean
----------------
Validates a boolean value.

__value__  
A value to validate.

__result__  
Returns #f if no errors were found, or one of the following symbols.

- 'wrong-type
- 'invalid-value

validate-integer
----------------
Validates an integer value.

__value__  
A value to validate.

__required?__  
Whether the value is required.

__min-value__  
The minimum allowed value.

__max-value__  
The maximum allowed value.

__result__  
Returns #f if no errors were found, or one of the following symbols.

- 'missing
- 'wrong-type
- 'too-low
- 'too-high
- 'invalid-value

validate-number
---------------
Validates a numeric value.

__value__  
A value to validate.

__required?__  
Whether the value is required.

__min-value__  
The minimum allowed value.

__max-value__  
The maximum allowed value.

__result__  
Returns #f if no errors were found, or one of the following symbols.

- 'missing
- 'wrong-type
- 'too-low
- 'too-high
- 'invalid-value

validate-string
---------------
Validates a string value.

__value__  
A value to validate.

__required?__  
Whether the value is required.

__min-length__  
The minimum allowed length.

__max-length__  
The maximum allowed length.

__result__  
Returns #f if no errors were found, or one of the following symbols.

- 'missing
- 'wrong-type
- 'too-short
- 'too-long
- 'invalid-value

validate-list
-------------
Validates a list value.

__value__  
A value to validate.

__required?__  
Whether the value is required.

__min-length__  
The minimum allowed length.

__max-length__  
The maximum allowed length.

__result__  
Returns #f if no errors were found, or one of the following symbols.

- 'missing
- 'wrong-type
- 'too-few
- 'too-many
- 'invalid-value

validate-record
---------------
Validates a record.

__value__  
A value to validate.

__required?__  
Whether the value is required.

__result__  
Returns #f if no errors were found, or one of the following symbols.

- 'missing
- 'invalid-value

raise-validation-errors-exception
---------------------------------
Raises a validation errors exception.

__validation-errors__  
A list of validation errors.

validation-errors-exception?
----------------------------
Returns whether an exception was caused by validation errors.

__exception__  
An exception.

validation-errors-exception-validation-errors
---------------------------------------------
Returns the validation errors from an exception.

__exception__  
An validation errors exception.

__result__  
A list of validation errors.

invalid-value
-------------
The symbol 'invalid-value always returns itself when validated.  
Useful if a value cound not be parsed for example.

try it
------
Place the following code in sources/main.scm.

    (declare (uses validation))

    (display (validate-integer 100 #t 1 10))
    (newline)
    (display (validate-number "hello" #t 1 10))
    (newline)
    (display (validate-string #f #t 1 10))
    (newline)

    (handle-exceptions
      exception
      (if (validation-errors-exception? exception)
        (begin
          (display (validation-errors-exception-validation-errors exception))
          (newline)))
      (raise-validation-errors-exception
        (list 'too-hot 'too-cold 'stinky)))

Run the following commands.

    $ make
    $ ./main

    too-high
    wrong-type
    missing
    (too-hot too-cold stinky)
