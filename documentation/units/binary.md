with-binary-packer
------------------
Invokes a procedure with a binary packer.

__procedure__  
A procedure invoked with the binary packer.

binary-packer-add-boolean
-------------------------
Adds a boolean value to a binary packer.

__binary-packer__  
A binary packer.

__value__  
A boolean value.

binary-packer-add-integer
-------------------------
Adds an integer value to a binary packer.

__binary-packer__  
A binary packer.

__value__  
An integer value.

binary-packer-add-double
------------------------
Adds a double value to a binary packer.

__binary-packer__  
A binary packer.

__value__  
A double value.

binary-packer-add-string
------------------------
Adds a string value to a binary packer.

__binary-packer__  
A binary packer.

__value__  
A string value.

binary-packer-add-bytes
-----------------------
Adds bytes to a binary packer.

__binary-packer__  
A binary packer.

__value__  
A u8vector value.

binary-packer-data
------------------
Returns the data from a binary packer.

__binary-packer__  
A binary packer.

__result__  
The packed binary data.

with-binary-unpacker
--------------------
Invokes a procedure with a binary unpacker.

__data__  
The packed binary data.

__procedure__  
A procedure invoked with the binary unpacker.

binary-unpacker-get-boolean
---------------------------
Returns an unpacked boolean.

__binary-unpacker__  
A binary unpacker.

__result__  
The unpacked boolean.

binary-unpacker-get-integer
---------------------------
Returns an unpacked integer.

__binary-unpacker__  
A binary unpacker.

__result__  
The unpacked integer.

binary-unpacker-get-double
--------------------------
Returns an unpacked double.

__binary-unpacker__  
A binary unpacker.

__result__  
The unpacked double.

binary-unpacker-get-string
--------------------------
Returns an unpacked string.

__binary-unpacker__  
A binary unpacker.

__result__  
The unpacked string.

binary-unpacker-get-bytes
-------------------------
Returns unpacked bytes.

__binary-unpacker__  
A binary unpacker.

__result__  
The unpacked u8vector.

try it
------
Place the following code in sources/main.scm.

    (import srfi-4)

    (declare (uses binary))

    (let ((binary-data
            (with-binary-packer
              (lambda (binary-packer)
                (binary-packer-add-boolean binary-packer #t)
                (binary-packer-add-integer binary-packer 123)
                (binary-packer-add-double binary-packer 123.45)
                (binary-packer-add-string binary-packer "ABC")
                (binary-packer-add-bytes binary-packer (make-u8vector 3 0))
                (binary-packer-data binary-packer)))))

      (display binary-data)
      (newline)

      (with-binary-unpacker
        binary-data
        (lambda (binary-unpacker)
          (display (binary-unpacker-get-boolean binary-unpacker))
          (newline)
          (display (binary-unpacker-get-integer binary-unpacker))
          (newline)
          (display (binary-unpacker-get-double binary-unpacker))
          (newline)
          (display (binary-unpacker-get-string binary-unpacker))
          (newline)
          (display (binary-unpacker-get-bytes binary-unpacker))
          (newline))))

Run the following commands.

    $ make
    $ ./main

    #u8(159 245 26 0 0 0 123 251 64 94 220 204 204 204 204 205 67 65 66 67 67 0 0 0 255)
    #t
    123
    123.45
    ABC
    #u8(0 0 0)

powered by
----------
The great libcbor.
