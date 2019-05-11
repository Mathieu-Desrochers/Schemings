read-configuration-file
-----------------------
Reads the configuration from a file and  
invokes a procedure with the root section node.

__file-name__  
A file name.

__procedure__  
A procedure accepting a configuration node.

configuration-section-get-value
-------------------------------
Returns a value from a section node.

__configuration-node__  
A configuration node.

__name__  
A setting name.

__result__  
The value of the setting.

configuration-section-get-section
---------------------------------
Returns a section node from a section node.

__configuration-node__  
A configuration node.

__name__  
A section name.

__result__  
The section node.

configuration-section-get-list
------------------------------
Returns a list node from a section node.

__configuration-node__  
A configuration node.

__name__  
A list name.

__result__  
The list node.

configuration-list-get-value
----------------------------
Returns a value from a list node.

__configuration-node__  
A configuration node.

__index__  
An index.

__result__  
The value at the index.

configuration-list-get-section
------------------------------
Returns a section node from a list node.

__configuration-node__  
A configuration node.

__index__  
An index.

__result__  
The section node at the index.

configuration-list-get-list
---------------------------
Returns a list node from a list node.

__configuration-node__  
A configuration node.

__index__  
An index.

__result__  
The list node at the index.

configuration-list-length
-------------------------
Returns a list node from a list node.

__configuration-node__  
A configuration node.

__result__  
The length of the configuration list.

try it
------
Place the following configuration in example.cfg.

    fire-power = 1000;

    evil-plan = {
      code-name = "icy snowball";
      secret-numbers = [1, 2, 8, 7];
    };

Place the following code in sources/main.scm.

    (declare (uses configuration))

    (read-configuration-file "example.cfg"
      (lambda (configuration-node)

        (display "Fire Power: ")
        (display
          (configuration-section-get-value configuration-node "fire-power"))
        (newline)

        (display "Evil Plan's Third Secret Number: ")
        (display
          (configuration-list-get-value
            (configuration-section-get-list
              (configuration-section-get-section configuration-node "evil-plan")
              "secret-numbers")
            2))
        (newline)))

Run the following commands.

    $ make
    $ ./main

    Fire Power: 1000
    Evil Plan's Third Secret Number: 8

powered by
----------
The great libconfig.
