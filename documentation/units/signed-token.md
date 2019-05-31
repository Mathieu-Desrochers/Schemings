sign-token
----------
Signs a token.

__token__  
A string token.

__secret__  
A secret.

__result__  
The signed token.

signed-token-validate
---------------------
Validates a signed token.

__signed-token__  
A signed token.

__secret__  
A secret.

__validity-in-seconds__  
A number of seconds during which the signed token is valid.

__result__  
The string token, or #f if the signed token in invalid.

try it
------
Place the following code in sources/main.scm.

    (declare (uses signed-token))

    (let ((signed-token (sign-token "alice@hotmail.com" "sUp3r-s3cReT")))

      (display signed-token)
      (newline)

      (display (signed-token-validate signed-token "sUp3r-s3cReT" 2))
      (newline)
      (sleep 3)
      (display (signed-token-validate signed-token "sUp3r-s3cReT" 2))
      (newline))

Run the following commands.

    $ make
    $ ./main

    alice@hotmail.com.2019-05-29T22:59:44Z.jt9WIWZRw4u2TC3XRpgw2tXZ04SUD+DJBp6OYRdsm6k=
    alice@hotmail.com
    #f
