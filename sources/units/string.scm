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
  (letrec ((string-find-replace-inner
             (lambda (string)
                (let ((find-index (string-contains string find)))
                  (if find-index
                    (string-find-replace-inner
                      (string-replace
                        string
                        replace
                        find-index (+ find-index (string-length find))
                        0 (string-length replace)))
                    string)))))
    (string-find-replace-inner string)))
