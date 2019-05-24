(import srfi-13)

(import (chicken condition))
(import (chicken format))

(declare (unit image))

(declare (uses exceptions))
(declare (uses file))
(declare (uses imagemagick))

;; initializes the image module
(: image-init (-> noreturn))
(define (image-init)
  (magick-wand-genesis))

;; returns whether a file contains an image
(: image-file? (string -> boolean))
(define (image-file? file-name)
  (let ((file-mime-type (file-mime-type file-name)))
    (or (string-contains file-mime-type "image/jpeg")
        (string-contains file-mime-type "image/png"))))

;; resizes an image
(: image-resize (string string fixnum fixnum -> noreturn))
(define (image-resize original-file-name output-file-name width height)
  (with-guaranteed-release
    new-magick-wand
    (lambda (magick-wand*)
      (unless (magick-read-image magick-wand* original-file-name)
        (abort
          (format "failed to read image ~A"
            original-file-name)))
      (let* ((original-width (magick-get-image-width magick-wand*))
             (original-height (magick-get-image-height magick-wand*))
             (width-ratio (/ width original-width))
             (height-ratio (/ height original-height))
             (resize-ratio (min width-ratio height-ratio)))
        (if (< resize-ratio 1)
          (unless
            (magick-resize-image
              magick-wand*
              (* original-width resize-ratio)
              (* original-height resize-ratio)
              magick-wand-lanczos-filter
              1.0)
            (abort
              (format "failed to resize image ~A"
                original-file-name)))))
      (unless (magick-write-images magick-wand* output-file-name #f)
        (abort
          (format "failed to write image ~A"
            output-file-name))))
    destroy-magick-wand))

;; releases the image module
(: image-release (-> noreturn))
(define (image-release)
  (magick-wand-terminus))
