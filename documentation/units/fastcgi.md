fastcgi-accept-requests
-----------------------
Starts accepting fastcgi requests.

__procedure__  
A procedure invoked with each fastcgi request.

fastcgi-request-authorization-header
------------------------------------
Returns the authorization header of a fastcgi request.

__fastcgi-request__  
A fastcgi request.

__result__  
The authorization header.

fastcgi-request-method
----------------------
Returns the method of a fastcgi request.

__fastcgi-request__  
A fastcgi request.

__result__  
The method.

fastcgi-request-uri
-------------------
Returns the uri of a fastcgi request.

__fastcgi-request__  
A fastcgi request.

__result__  
The uri.

fastcgi-read-request-body
-------------------------
Reads the body of a fastcgi request.

__fastcgi-request__  
A fastcgi request.

__result__  
The body as a blob.

fastcgi-write-response-line
---------------------------
Writes a string followed by a line feed to a fastcgi request.

__fastcgi-request__  
A fastcgi request.

__string__  
A string to be written.

fastcgi-write-response-blob
---------------------------
Writes a blob to a fastcgi request.

__fastcgi-request__  
A fastcgi request.

__blob__  
A blob to be written.

try it
------
Place the following code in sources/main.scm.

    (import srfi-13)

    (declare (uses fastcgi))

    (fastcgi-accept-requests
      (lambda (fastcgi-request)
        (fastcgi-write-response-line fastcgi-request "Status: 200 OK")
        (fastcgi-write-response-line fastcgi-request "")
        (fastcgi-write-response-line fastcgi-request
          (string-append
            "Your request was "
            (fastcgi-request-method fastcgi-request) " "
            (fastcgi-request-uri fastcgi-request) "."))
        (fastcgi-write-response-line fastcgi-request
          "Bee seeing you...")))

Run the following commands.

    $ make
    $ spawn-fcgi -p 9000 main

Place the following configuration in httpd.conf.

    server "default" {
      listen on 0.0.0.0 port 80
      fastcgi socket tcp 127.0.0.1 9000
    }

Start the httpd server and run the following command.

    $ curl -i 'http://localhost/doughnuts'

    HTTP/1.1 200 OK
    Connection: keep-alive
    Date: Sun, 23 May 2021 14:36:11 GMT
    Server: OpenBSD httpd
    Transfer-Encoding: chunked
    
    Your request was GET /doughnuts.
    Bee seeing you...

powered by
----------
The great fastcgi.
