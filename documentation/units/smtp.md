with-smtp-connection
--------------------
Invokes a procedure with a smtp connection.

__server__  
A server name.

__port__  
A server port.

__user__  
A user.

__password__  
A password.

__procedure__  
A procedure invoked with the smtp connection.

smtp-send
---------
Sends an email.

__smtp-connection__  
A smtp connection.

__email__  
An email.

try it
------
Place the following code in sources/main.scm.

    (declare (uses email))
    (declare (uses smtp))

    (with-smtp-connection "smtp.live.com" 587 "alice@hotmail.com" "p@$$w0rd"
      (lambda (smtp-connection)
        (smtp-send
          smtp-connection
          (make-email
            "alice@hotmail.com"
            "complaints-department@hotmail.com"
            "bob@hotmail.com"
            "ALERT: Candies in the kitchen"
            "No kidding.\nFirst come first served.\n\nAlice"
            "<html><body>We have <b>candies</b> in the kitchen.</body></html>"
            (list)))))

Run the following commands.

    $ make
    $ ./main

powered by
----------
The great libetpan.
