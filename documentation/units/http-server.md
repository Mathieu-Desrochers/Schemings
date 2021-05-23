http-binding
------------
A record with the following fields.  
Used to register a service to the http server.

- method
- route-regex
- request-content-type
- response-content-type
- service-procedure
- parse-request-procedure
- format-response-procedure
- requires-authentication

__request-content-type__  
Must be one of the following values or false.

- "application/json; charset=utf-8"
- "application/octet-stream"

__response-content-type__  
Must be one of the following values or false.

- "application/json; charset=utf-8"
- "application/octet-stream"
- "application/pdf"

__service-procedure__  
Must accept the following parameters.

- request
- sql-connection
- jobs-queue-connection
- authentication
- configuration

__parse-request-procedure__  
Must accept the following parameters.

- route-captures
- request-body

__format-response-procedure__  
Must return the response body in one of these formats.

- string
- blob

http-server-request
-------------------
A record with the following fields.

- method
- uri
- body

The server keeps track of the current request in the following global variable.  
Useful when included in crash logs.

    http-server-request-current

http-server-start
-----------------
Starts serving requests.  
This is a blocking call.

__http-bindings__  
A list of http-bindings.

__get-authentication-procedure__  
A procedure invoked with each fastcgi-request.  
Must return any attached authentication.

__sql-connection__  
A sql-connection of false.

__jobs-queue-connection__  
A jobs-queue-connection of false.

__configuration__  
Any configuration to be passed to the services procedure.

sql-connection
--------------
The same sql-connection is used across all service calls.  
Each call is automatically isolated inside a sql-transaction.

http response codes
-------------------
The following response codes are automatically returned.

- no matching http-binding-route: 404 Not Found
- parse-request-procedure returned false: 400 Bad Request
- get-authentication-procedure returned false: 401 Unauthorized
- service-procedure invoked raise-validation-errors-exception: 422 Unprocessable Entity
- response-content-type is false: 204 No Content
- otherwise: 200 OK

try it
------
Place the following code in sources/main.scm.

    (declare (uses http-server))
    (declare (uses json))

    ;; define a simple service
    ;; would be part of your application code
    (define-record get-dog-request id)
    (define-record get-dog-response greeting)
    (define
      (get-dog-service get-dog-request sql-connection jobs-queue-connection authentication configuration)
      (make-get-dog-response "woof"))

    ;; make an http-binding for the service
    (let ((get-dog-http-binding
            (make-http-binding
              "GET"
              "^/dogs/(\\d{1,6})$"
              #f
              "application/json; charset=utf-8"
              get-dog-service
              (lambda (route-captures request-body)
                (make-get-dog-request
                  (string->number (car route-captures))))
              (lambda (get-dog-response)
                (get-dog-response-greeting
                  get-dog-response))
              #f)))

      ;; start serving http requests
      (http-server-start
        (list
          get-dog-http-binding)
        #f
        #f
        #f
        #f))

Run the following commands.

    $ make
    $ spawn-fcgi -p 9000 main

Place the following configuration in httpd.conf.

    server "default" {
      listen on 0.0.0.0 port 80
      fastcgi socket tcp 127.0.0.1 9000
    }

Start the httpd server and run the following command.

    $ curl -i 'http://localhost/dogs/1'

    HTTP/1.1 200 OK
    Content-Type: application/json; charset=utf-8
    Content-Length: 6
    Date: Sat, 01 Jun 2019 22:02:02 GMT
    Server: lighttpd/1.4.53

    woof

    $ curl -i 'http://localhost/cats'

    HTTP/1.1 404 Not Found
    Content-Length: 0
    Date: Sat, 01 Jun 2019 21:46:43 GMT
    Server: lighttpd/1.4.53
