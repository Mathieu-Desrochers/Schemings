(import srfi-4)
(import srfi-13)

(import (chicken file posix))
(import (chicken io))
(import (chicken process))

(declare (unit file*))

(declare (uses crypto))
(declare (uses exceptions))

;; generates a unique file name
(: file*-unique-name (string -> string))
(define (file*-unique-name prefix)
  (string-append
    prefix
    (number->string (crypto-random-number 999999)) "-"
    (number->string (crypto-random-number 999999)) "-"
    (number->string (crypto-random-number 999999)) "-"
    (number->string (crypto-random-number 999999))))

;; returns the mime type of a file
(: file*-mime-type (string -> string))
(define (file*-mime-type file-name)
  (let-values (((input-port output-port process-id) (process "file" (list "-i" file-name))))
    (let* ((line (read-line input-port))
           (column-index (string-index line #\:)))
      (string-drop line (+ column-index 2)))))

;; loads a file
(: file*-load (string -> blob))
(define (file*-load file-name)
  (with-guaranteed-release
    (lambda ()
      (file-open file-name open/rdonly))
    (lambda (file-descriptor)
      (let ((file-port (open-input-file* file-descriptor)))
        (u8vector->blob
          (read-u8vector (file-size file-descriptor) file-port))))
    file-close))

;; saves a file
(: file*-save (string blob -> noreturn))
(define (file*-save file-name content)
  (with-guaranteed-release
    (lambda ()
      (file-open file-name (+ open/wronly open/creat)))
    (lambda (file-descriptor)
      (file-write file-descriptor content))
    file-close))
