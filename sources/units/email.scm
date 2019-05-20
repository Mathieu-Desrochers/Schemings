(declare (unit email))

(declare (uses crypto))
(declare (uses email-intern))
(declare (uses etpan))
(declare (uses exceptions))

;; encapsulates an email
(define-typed-record email
  (from string)
  (to string)
  (subject string)
  (body string)
  (attachments (list-of (struct email-attachment))))

;; encapsulates an email attachment
(define-typed-record email-attachment
  (content-type string)
  (file-name string)
  (content blob))

;; converts an email to string
(: email->string ((struct email) -> string))
(define (email->string email)
  (with-message-mailmime*
    (lambda (message-mailmime*)
      (mailmime*-set-email-fields email message-mailmime*)
      (let ((mime-part-multiple (add-multiple-mailmime* message-mailmime* (crypto-random-string 24))))
        (add-text-mailmime* mime-part-multiple (email-body email))
        (for-each
          (lambda (email-attachment)
            (add-attachment-mailmime*
              mime-part-multiple
              (email-attachment-content-type email-attachment)
              (email-attachment-file-name email-attachment)
              (email-attachment-content email-attachment)))
          (email-attachments email))
        (mailmime-write-file message-mailmime*)))))
