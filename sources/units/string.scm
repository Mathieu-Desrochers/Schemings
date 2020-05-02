(import srfi-13)

(declare (unit string))

;; returns a specified number of characters from a string
(: string-mid (string fixnum fixnum -> string))
(define (string-mid string start length)
  (if (< start (string-length string))
    (substring/shared
      string
      start
      (min (+ start length) (string-length string)))
    ""))

;; finds and replaces within a string
(: string-find-replace (string string string -> string))
(define (string-find-replace string find replace)
  (letrec* (
      (string-length-find (string-length find))
      (string-length-replace (string-length replace))
      (string-find-replace-inner
        (lambda (string accumulated-index)
          (let ((index (string-contains string find accumulated-index)))
            (if index
              (string-find-replace-inner
                (string-replace
                  string
                  replace
                  index
                  (+ index string-length-find))
                (+ index string-length-replace))
              string)))))
    (if (> string-length-find 0)
      (string-find-replace-inner string 0)
      string)))
