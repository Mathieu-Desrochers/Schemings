image-init
----------
Initializes the image module.  
Make sure to invoke this procedure before  
any other ones in this unit.

image-file?
-----------
Returns whether a file contains an image.

__file-name__  
An image file name.

__result__  
Whether the file contains an image.

image-resize
------------
Resizes an image.

__original-file-name__  
An original image file name.

__output-file-name__  
A resized image file name.

__width__  
A width in pixels.

__height__  
A height in pixels.

image-release
-------------
Releases the image module.

try it
------
Save your favorite dog picture to dog.jpg.

Place the following code in sources/main.scm.

    (declare (uses image))

    (image-init)
    (image-resize "dog.jpg" "smol-doggo.jpg" 100 100)
    (image-release)

Run the following commands.

    $ make
    $ ./main

powered by
----------
The great ImageMagick.
