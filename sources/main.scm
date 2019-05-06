(import (chicken blob))

(declare (uses base64))

(display (base64-encode (make-blob 1000)))
(newline)
