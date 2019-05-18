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
    $ cp main /tmp

Place the following configuration in lighttpd.conf.

    server.modules += ( "mod_fastcgi" )

    fastcgi.server += (
      "/main/" =>
      ( "main" =>
        (
          "bin-path" => "/tmp/main",
          "check-local" => "disable",
          "socket" => "/tmp/main-socket"
        )
      )
    )

Start the lighttpd server and  
run the following command.

    $ curl -i 'http://localhost/main/pizza'

    HTTP/1.1 200 OK
    Content-Length: 54
    Date: Sat, 18 May 2019 19:01:20 GMT
    Server: lighttpd/1.4.53

    Your request was GET /main/pizza.
    Bee seeing you...

powered by
----------
The great libfcgi.
