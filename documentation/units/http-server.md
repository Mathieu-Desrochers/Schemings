http-server-binding
-------------------
A record with the following fields.

__method__  
An http method.

__route__  
A route expressed as a regex.

__request-content-type__  
A content type for the request.  
Must be one of the following values.  
Can be #f if the request does not expect a body.

- "application/json; charset=utf-8"
- "application/octet-stream"

__response-content-type__  
A content type for the response.  
Must be one of the following values.  
Can be #f if the response does not contain a body.

- "application/json; charset=utf-8"
- "application/octet-stream"
- "application/pdf"

__service__  
A service procedure.  
Must accept the following parameters.

- request
- sql-connection
- authentication
- configuration

__parse-request__  
A parse request procedure.  
Receives a list of captures from the route regex and the request body.  
Must return false if the request cannot be parsed.

__format-response__  
A format response procedure.  
Must return the response body as a string or a blob.

__requires-authentication__  
Whether authentication is required.

http-server-start
-----------------
Starts serving requests.  
This is a blocking call.

__http-bindings__  
A list of http-bindings.

__get-authentication-procedure__  
A procedure invoked for the http-bindings requiring authentication.  
Must return the authentication attached to a fast-cgi request,  
or false if the authentication is missing or invalid.

__sql-connection__  
An opened sql-connection of false.  
Will be passed as a parameter to the service procedure.  
The service calls are automatically isolated by sql-transactions.

__configuration__  
Any configuration to be passed to the services procedure.  
Will be passed as a parameter to the service procedure.

try it
------
Place the following code in sources/main.scm.

    (declare (uses http-server))
    (declare (uses json))

    ;; define a simple service
    ;; this would be part of your application code
    (define-record get-dog-request id)
    (define-record get-dog-response greeting)
    (define (get-dog-service get-dog-request sql-connection authentication configuration)
      (make-get-dog-response "woof"))

    ;; make an http-binding for the service
    (let ((get-dog-http-binding
            (make-http-binding
              "GET" "^/main/dogs/(\\d{1,6})$" #f "application/json; charset=utf-8"
              get-dog-service
              (lambda (route-captures request-body)
                (make-get-dog-request
                  (string->number (car route-captures))))
              (lambda (get-dog-response)
                (with-json-object
                  (lambda (json-node)
                    (json-object-add-value json-node "greeting"
                      (get-dog-response-greeting get-dog-response))
                    (json->string json-node))))
              #f)))

      ;; start serving http requests
      (http-server-start
        (list get-dog-http-binding)
        #f
        #f
        #f))

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

Start the lighttpd server and run the following command.

    $ curl -i 'http://localhost/main/dogs/1'

    HTTP/1.1 200 OK
    Content-Type: application/json; charset=utf-8
    Content-Length: 26
    Date: Sat, 01 Jun 2019 21:46:39 GMT
    Server: lighttpd/1.4.53

    {
      "greeting": "woof"
    }

    $ curl -i 'http://localhost/main/cats'

    HTTP/1.1 404 Not Found
    Content-Length: 0
    Date: Sat, 01 Jun 2019 21:46:43 GMT
    Server: lighttpd/1.4.53
