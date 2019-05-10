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
Place the following code in sources/main.scm.

    (import (chicken blob))

    (declare (uses base64))

    (display (base64-encode (make-blob 16)))
    (newline)

Run the following commands.

    $ make
    $ ./main

    cHRLAAgAAAAwpvP//38AAA==

powered by
----------
The great libb64.
