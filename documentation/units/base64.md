base64-encode
-------------
Encodes bytes to base64.

__bytes__  
A bytes vector.

    (make->blob 16)

__result__  
The base64 encoded bytes.

    cmFudGVlZC0AAAAAAAAAAA==

base64-decode
-------------
Decodes bytes from base64.

__string__  
A base64 string.

    cmFudGVlZC0AAAAAAAAAAA==

__result__  
The decoded bytes.

    #${72616e746565642d0000000000000000}
