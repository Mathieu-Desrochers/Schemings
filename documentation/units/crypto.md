crypto-init
-----------
Initializes the crypto module.  
Make sure to invoke this procedure before  
any other ones in this unit.

crypto-random-number
--------------------
Returns a random number.

__upper-bound__  
An exclusive maximum value.

__result__  
The random number.  
Chosen by fair dice roll.  
Guaranteed to be random.

crypto-random-string
--------------------
Returns a random string.

__length__  
A string length.

__result__  
The random string.

crypto-hash-bytes
-----------------
Computes the hash of bytes.

__bytes__  
A blob of bytes.

__secret__  
A string known only to the application.

__result__  
The hash of the bytes encoded in base64.

crypto-hash-string
------------------
Same as above but for a string.

try it
------
Place the following code in sources/main.scm.

    (declare (uses crypto))

    (crypto-init)

    (display (crypto-random-number 100))
    (newline)

    (display (crypto-hash-string "Guess this should be signed." "sUp3r-s3cReT"))
    (newline)

Run the following commands.

    $ make
    $ ./main

    24
    KC1QR32cw1F1ZsmDqvPfo4josdpe/xXWdZfOmOLCsF8=

powered by
----------
The great libsodium.
