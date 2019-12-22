http-client-request
-------------------
A record with the following fields.

- method
- url
- username
- password
- headers
- body

http-client-response
--------------------
A record with the following fields.

- status-code
- body

http-client-init
----------------
Initializes the http client unit.  
Make sure to invoke this procedure before  
any other ones in this unit.

http-client-url-encode
----------------------
Url encodes a string.

__string__  
An string.

__result__  
The encoded string.

http-client-perform
-------------------
Performs an http client request.

__http-client-request__  
An http client request.

__result__  
The http client response.

http-client-cleanup
-------------------
Cleans up the http client unit.

try it
------
Place the following code in sources/main.scm.

    (declare (uses http-client))

    (http-client-init)

    (let ((http-client-response
            (http-client-perform
              (make-http-client-request
                "GET"
                "https://reqres.in/api/users/1"
                #f #f #f #f))))

      (display (http-client-response-status-code http-client-response))
      (newline)
      (display (http-client-response-body http-client-response))
      (newline))

    (http-client-cleanup)

Run the following commands.

    $ make
    $ ./main

    200
    {"data":{"id":1,"email":"george.bluth@reqres.in", ...}}

powered by
----------
The great libcurl.
