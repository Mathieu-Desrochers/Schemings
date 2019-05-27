file-unique-name
----------------
Generates a unique file name.

__prefix__  
A prefix.

__result__  
The unique file name.

file-mime-type
--------------
Returns the mime type of a file.

__file-name__  
A file name.

__result__  
The mime type of the file.

file-load
---------
Loads a file.

__file-name__  
A file name.

__result__  
A blob containing the file content.

file-save
---------
Saves a file.

__file-name__  
A file name.

__content__  
A blob containing the file content.

try it
------
Save your favorite dog picture to dog.jpg.

Place the following code in sources/main.scm.

    (declare (uses file))

    (display (file-unique-name "/tmp/report-"))
    (newline)

    (display (file-mime-type "dog.jpg"))
    (newline)

    (file-save "backup-dog.jpg" (file-load "dog.jpg"))

Run the following commands.

    $ make
    $ ./main

    /tmp/report-378545-418744-645449-268866
    image/jpeg; charset=binary
