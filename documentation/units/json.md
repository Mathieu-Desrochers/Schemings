with-json-object-node
---------------------
Invokes a procedure with a new object node.

__procedure__  
A procedure accepting the object node.

with-json-array-node
--------------------
Invokes a procedure with a new array node.

__procedure__  
A procedure accepting the array node.

json-object-add-value
---------------------
Adds a value node to an object node.

__json-node__  
An object node.

__property-name__  
The name of the property.

__property-value__  
The value of the property.

json-object-add-object
----------------------
Adds an object node to an object node.

__json-node__  
An object node.

__property-name__  
The name of the property.

__result__  
The new object node.

json-object-add-array
---------------------
Adds an array node to an object node.

__json-node__  
An object node.

__property-name__  
The name of the property.

__result__  
The new array node.

json-array-add-value
--------------------
Adds a value node to an array node.

__json-node__  
An array node.

__value__  
The value.

json-array-add-object
---------------------
Adds an object node to an array node.

__json-node__  
An array node.

__result__  
The new object node.

json-array-add-array
--------------------
Adds an array node to an array node.

__json-node__  
An array node.

__result__  
The new array node.

json->string
------------
Formats a json node to string.

__json-node__  
A json node.

__result__  
The formatted string.

with-string->json
-----------------
Invokes a procedure with a parsed json node.

__string__  
A json string.

__procedure__  
A procedure accepting the json node.

json-object-get-value
---------------------
Returns a value from an object node.

__json-node__  
An object node.

__property-name__  
A property name.

__result__  
The value of the property.

json-object-get-object
----------------------
Returns an object node from an object node.

__json-node__  
An object node.

__property-name__  
A property name.

__result__  
The object node of the property.

json-object-get-array
---------------------
Returns an array node from an object node.

__json-node__  
An object node.

__property-name__  
A property name.

__result__  
The array node of the property.

json-array-get-value
--------------------
Returns a value from an array node.

__json-node__  
An array node.

__index__  
An index.

__result__  
The value at the index.

json-array-get-object
---------------------
Returns an object node from an array node.

__json-node__  
An array node.

__index__  
An index.

__result__  
The object node at the index.

json-array-get-array
--------------------
Returns an array node from an array node.

__json-node__  
An array node.

__index__  
An index.

__result__  
The array node at the index.

json-array-length
-----------------
Returns the length of an array node.

__json-node__  
An array node.

__result__  
The length of the array node.

try it
------
Place the following code in sources/main.scm.


Run the following commands.

    $ make
    $ ./main


powered by
----------
The great libjansson.
