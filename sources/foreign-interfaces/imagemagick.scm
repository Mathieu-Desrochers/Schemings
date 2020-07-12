(declare (unit imagemagick))

(foreign-declare "

#include <MagickWand/MagickWand.h>

")

;; filter codes
(define magick-wand-lanczos-filter (foreign-value "LanczosFilter" int))

;; MagickWand pointers definitions
(define-foreign-type magick-wand* (c-pointer "MagickWand"))

;; initializes and terminates the MagickWand environment
(define magick-wand-genesis (foreign-lambda void "MagickWandGenesis"))
(define magick-wand-terminus (foreign-lambda void "MagickWandTerminus"))

;; creates and destroys a wand
(define new-magick-wand (foreign-lambda magick-wand* "NewMagickWand"))
(define destroy-magick-wand (foreign-lambda magick-wand* "DestroyMagickWand" magick-wand*))

;; reads and writes an image
(define magick-read-image (foreign-lambda bool "MagickReadImage" magick-wand* c-string))
(define magick-write-images (foreign-lambda bool "MagickWriteImages" magick-wand* c-string bool))

;; returns the image size
(define magick-get-image-height (foreign-lambda int "MagickGetImageHeight" magick-wand*))
(define magick-get-image-width (foreign-lambda int "MagickGetImageWidth" magick-wand*))

;; scales the current active image to the desired dimensions
(define magick-resize-image (foreign-lambda bool "MagickResizeImage" magick-wand* int int int))
