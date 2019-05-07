with-exception-hiding
---------------------
Invokes a procedure and hides any exception it raises.

__procedure__  
A procedure that does shady things.

with-guaranteed-release
-----------------------
Makes sure a resource is released even in the face of an exception.

__allocation-procedure__  
A procedure that allocates and returns a resource.

__procedure__  
A procedure invoked with the allocated resource.  
It can perform work without worrying about its release.

__release-procedure__  
A procedure invoked with the allocated resource.  
It is responsible for its release.

example
-------
Run the following commands.

    $ vim sources/main.scm

    (declare (uses exceptions))

    (with-guaranteed-release
      (lambda () "expensive resource")
      (lambda (resource) (/ 1 0))
      (lambda (resource)
        (display (string-append "releasing " resource))
        (newline)))

    $ make
    $ ./main

    releasing expensive resource

    Error: (/) division by zero
    1
    0

    Call history:
    ...
