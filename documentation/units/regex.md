with-compiled-regex
-------------------
Invokes a procedure with a compiled regular expression.

__pattern__  
A regular expression pattern.

__procedure__  
A procedure accepting a compiled regular expression.

regex-execute-compiled
----------------------
Executes a compiled regular expression.

__regex__  
A compiled regular expression.

__string__  
A string to match.

__result__  
The matches or an empty list.

regex-execute
-------------
Executes a regular expression.

__pattern__  
A regular expression pattern.

__string__  
A string to match.

__result__  
The matches or an empty list.

try it
------
Place the following code in sources/main.scm.

    (declare (uses regex))

    (with-compiled-regex
      "^/secret-societies/(\\d{1,6})$"
      (lambda (regex)
        (display (regex-execute-compiled regex "/secret-societies/123"))
        (newline)
        (display (regex-execute-compiled regex "/secret-societies/45"))
        (newline)))

    (display (regex-execute ".*?(\\d+).*" "abc123xyz"))
    (newline)

Run the following commands.

    $ make
    $ ./main

    (/secret-societies/123 123)
    (/secret-societies/45 45)
    (abc123xyz 123)

powered by
----------
The great libpcre.
