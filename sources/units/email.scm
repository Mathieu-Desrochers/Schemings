(declare (unit email))

(declare (uses crypto))
(declare (uses email-intern))
(declare (uses etpan))
(declare (uses exceptions))

;; encapsulates an email
(define-typed-record email
  (from string)
  (reply-to (or string false))
  (to string)
  (subject string)
  (body-text (or string false))
  (body-html (or string false))
  (attachments (list-of (struct email-attachment))))

;; encapsulates an email attachment
(define-typed-record email-attachment
  (content-type string)
  (file-name string)
  (content blob))

;; converts an email to string
(: email->string ((struct email) -> string))
(define (email->string email)
  (let ((add-text-mailmimes*
          (lambda (parent-mailmime*)
            (if (and (email-body-text email) (email-body-html email))
              (let ((mime-part-alternative*
                      (add-multiple-mailmime*
                        parent-mailmime* "multipart/alternative" (crypto-random-string 24))))
                (add-text-mailmime* mime-part-alternative* "text/plain" (email-body-text email))
                (add-text-mailmime* mime-part-alternative* "text/html" (email-body-html email)))
              (if (email-body-text email)
                (add-text-mailmime* parent-mailmime* "text/plain" (email-body-text email))
                (add-text-mailmime* parent-mailmime* "text/html" (email-body-html email)))))))
    (with-message-mailmime*
      (lambda (message-mailmime*)
        (mailmime*-set-email-fields email message-mailmime*)
        (if (not (null? (email-attachments email)))
          (let ((mime-part-mixed*
                  (add-multiple-mailmime*
                    message-mailmime* "multipart/mixed" (crypto-random-string 24))))
            (add-text-mailmimes* mime-part-mixed*)
            (for-each
              (lambda (email-attachment)
                (add-attachment-mailmime*
                  mime-part-mixed*
                  (email-attachment-content-type email-attachment)
                  (email-attachment-file-name email-attachment)
                  (email-attachment-content email-attachment)))
              (email-attachments email)))
          (add-text-mailmimes* message-mailmime*))
        (mailmime-write-file message-mailmime*)))))
