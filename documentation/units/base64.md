base64-encode
-------------
Encodes bytes to base64.

__bytes__  
A blob of bytes.

__result__  
The base64 string.

base64-decode
-------------
Decodes bytes from base64.

__string__  
A base64 string.

__result__  
The blob of bytes.

try it
------
Run the following commands.

    $ vim sources/main.scm

    (import (chicken blob))

    (declare (uses base64))

    (display (base64-encode (make-blob 16)))
    (newline)

    $ make
    $ ./main

    cHRLAAgAAAAwpvP//38AAA==
    #${70744b000800000030a6f3ffff7f0000}

powered by
----------
The great libb64.
