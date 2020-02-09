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

binary-unpacker-boolean
-----------------------
Returns an unpacked boolean.

__binary-unpacker__  
A binary unpacker.

__result__  
The unpacked boolean.

binary-unpacker-integer
-----------------------
Returns an unpacked integer.

__binary-unpacker__  
A binary unpacker.

__result__  
The unpacked integer.

binary-unpacker-double
----------------------
Returns an unpacked double.

__binary-unpacker__  
A binary unpacker.

__result__  
The unpacked double.

binary-unpacker-string
----------------------
Returns an unpacked string.

__binary-unpacker__  
A binary unpacker.

__result__  
The unpacked string.

try it
------
Place the following code in sources/main.scm.

    (declare (uses binary))

    (let ((packed-binary-data
            (with-binary-packer
              (lambda (binary-packer)
                (binary-packer-add-boolean binary-packer #t)
                (binary-packer-add-int binary-packer 123)
                (binary-packer-add-double binary-packer 123.45)
                (binary-packer-add-string binary-packer "You hungry?")
                (binary-packer-data binary-packer)))))

      (display packed-binary-data)
      (newline)

      (with-binary-unpacker
        packed-binary-data
        (lambda (binary-unpacker)
          (display (binary-unpacker-boolean binary-unpacker))
          (newline)
          (display (binary-unpacker-int binary-unpacker))
          (newline)
          (display (binary-unpacker-double binary-unpacker))
          (newline)
          (display (binary-unpacker-string binary-unpacker))
          (newline))))

Run the following commands.

    $ make
    $ ./main

    #u8(195 123 203 64 94 220 204 204 204 204 205 171 89 111 117 32 104 117 110 103 114 121 63)
    #t
    123
    123.45
    You hungry?

powered by
----------
The great msgpack-c.
