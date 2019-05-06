with-exception-hiding
---------------------
Invokes a procedure and hides any exception it raises.

__invocation__

    (with-exception-hiding
      (lambda ()
        (/ 1 0)))

    (display "Still running")

__output__

    Still running

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

__invocation__

    (with-guaranteed-release
      (lambda () "expensive resource")
      (lambda (resource) (/ 1 0))
      (lambda (resource) (display (string-append "releasing " resource))))

__output__

    releasing expensive resource
