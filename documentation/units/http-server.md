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
Can be false if the request does not expect a body.

- "application/json; charset=utf-8"
- "application/octet-stream"

__response-content-type__  
A content type for the response.  
Must be one of the following values.  
Can be false if the response does not contain a body.

- "application/json; charset=utf-8"
- "application/octet-stream"
- "application/pdf"

__service-procedure__  
A procedure that will handle requests.  
Must accept the following parameters.

- request
- sql-connection
- authentication
- configuration

__parse-request-procedure__  
A procedure used to parse requests.  
Receives a list of captures from the route regex and the request body.  
Must return false if the request cannot be parsed.

__format-response-procedure__  
A procedure used to format responses.  
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

http response codes
-------------------
The following response codes are automatically returned.

- no matching http-binding-route: 404 Not Found
- parse-request-procedure returned false: 400 Bad Request
- get-authentication-procedure returned false: 401 Unauthorized
- service-procedure raised a validation-errors-exception: 422 Unprocessable Entity
- response-content-type is false: 204 No Content
- otherwise: 200 OK

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
                (get-dog-response-greeting get-dog-response))
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
    Content-Length: 6
    Date: Sat, 01 Jun 2019 22:02:02 GMT
    Server: lighttpd/1.4.53

    woof

    $ curl -i 'http://localhost/main/cats'

    HTTP/1.1 404 Not Found
    Content-Length: 0
    Date: Sat, 01 Jun 2019 21:46:43 GMT
    Server: lighttpd/1.4.53
