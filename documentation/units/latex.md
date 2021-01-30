latex-escape
------------
Escapes the latex characters in a string.

__string__  
A string to escape.

__result__  
The escaped string.

latex-number->string-with-fractions
-----------------------------------
Formats a number to a latex string with fractions.

__number__  
A number.

__result__  
An string with fractions.

latex-headers
-------------
Returns latex headers that produce a nice layout.

__result__  
The latex headers.

latex-headers-legal
-------------------
Returns latex headers that produce a nice legal layout.

__result__  
The latex headers.

latex-headers-landscape
-----------------------
Returns latex headers that produce a nice landscape layout.

__result__  
The latex headers.

latex-headers-absolute-positioning
----------------------------------
Returns latex headers that produce an absolute positioning friendly layout.

__result__  
The latex headers.

latex-generate-pdf
------------------
Generates a pdf file from a latex source.

__latex-source__  
A latex source.

__output-file-name__  
An output file name.

try it
------
Place the following code in sources/main.scm.

    (declare (uses latex))

    (display (latex-escape "100.00$"))
    (newline)

    (latex-generate-pdf
      (string-append
        (latex-headers)
        "\\begin{document}\n"
        "Hello, is it me you are looking for?"
        "\\end{document}\n")
      "/tmp/message.pdf")

Run the following commands.

    $ make
    $ ./main

    100.00\$

    $ firefox /tmp/message.pdf

powered by
----------
The great texlive.
