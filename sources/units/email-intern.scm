(import srfi-4)

(import (chicken blob))
(import (chicken condition))

(declare (unit email-intern))

(declare (uses etpan))

;; invokes a procedure with a mailmime* of type message
(: with-message-mailmime* (forall (r) ((pointer -> r) -> r)))
(define (with-message-mailmime* procedure)
  (with-guaranteed-release
    (lambda ()
      (mailmime-new-message-data #f))
    procedure
    mailmime-free))

;; makes a mailmime* of type single
(: make-single-mailmime* (
   string (or string false) (or string false) fixnum (or string false) blob fixnum ->
   pointer))
(define (make-single-mailmime*
          content-type
          content-type-parameter-name
          content-type-parameter-value
          content-disposition
          content-disposition-name
          content
          content-length)
  (let ((mailmime-disposition*
          (mailmime-disposition-new-with-data
            content-disposition
            (if content-disposition-name
              (mailmime-strdup content-disposition-name)
              #f)
            #f #f #f -1)))
    (unless mailmime-disposition*
      (abort "failed to create mailmime-disposition"))
    (let ((mailmime-mechanism*
            (mailmime-mechanism-new mailmime-mechanism-base64 #f)))
      (unless mailmime-mechanism*
        (abort "failed to create mailmime-mechanism"))
      (let ((mailmime-fields*
              (mailmime-fields-new-with-data mailmime-mechanism* #f #f mailmime-disposition* #f)))
        (unless mailmime-fields*
          (abort "failed to create mailmime-fields"))
        (let ((mailmime-content*
                (mailmime-content-new-with-str content-type)))
          (unless mailmime-content*
            (abort "failed to create mailmime-content"))
          (if (and content-type-parameter-name content-type-parameter-value)
            (let ((mailmime-parameter*
                    (mailmime-param-new-with-data
                      content-type-parameter-name
                      content-type-parameter-value)))
              (unless mailmime-parameter*
                (abort"failed to create mailmime-parameter"))
              (unless (eq? (mailmime-content-add-parameter mailmime-content* mailmime-parameter*) 0)
                (abort "failed to append mailmime-parameter"))))
          (let ((mailmime*
                  (mailmime-new
                    mailmime-single #f 0
                    mailmime-fields* mailmime-content*
                    #f #f #f #f #f #f)))
            (unless mailmime*
              (abort "failed to create mailmime"))
            (unless (eq? (mailmime-set-body-text mailmime* (blob->u8vector content) content-length) 0)
              (abort "failed to set mailmime body"))
            mailmime*))))))

;; adds a mailmime* of type text
;; to a parent mailmime*
(: add-text-mailmime* (pointer string -> noreturn))
(define (add-text-mailmime* parent-mailmime* text)
  (let* ((content (string->blob text))
         (mailmime*
            (make-single-mailmime*
              "text/plain"
              "charset"
              "utf-8"
              mailmime-disposition-type-inline
              #f
              content
              (blob-size content))))
    (unless (eq? (mailmime-add-part parent-mailmime* mailmime*) 0)
      (abort "failed to add mailmime part"))))

;; adds a mailmime* of type attachment
;; to a parent mailmime*
(: add-attachment-mailmime* (pointer string string blob -> noreturn))
(define (add-attachment-mailmime* parent-mailmime* content-type file-name content)
  (let ((mailmime*
          (make-single-mailmime*
            content-type
            #f
            #f
            mailmime-disposition-type-attachment
            file-name
            content
            (blob-size content))))
    (unless (eq? (mailmime-add-part parent-mailmime* mailmime*) 0)
      (abort "failed to add mailmime part"))))

;; adds a mailmime* of type multiple
;; to a parent mailmime*
(: add-multiple-mailmime* (pointer string -> pointer))
(define (add-multiple-mailmime* parent-mailmime* boundary)
  (let ((mailmime-fields*
          (mailmime-fields-new-empty)))
    (unless mailmime-fields*
      (abort "failed to create mailmime-fields"))
    (let ((mailmime-content*
            (mailmime-content-new-with-str "multipart/mixed")))
      (unless mailmime-content*
        (abort "failed to create mailmime-content"))
      (let ((mailmime-parameter*
              (mailmime-param-new-with-data "boundary" boundary)))
        (unless mailmime-parameter*
          (abort"failed to create mailmime-parameter"))
        (unless (eq? (mailmime-content-add-parameter mailmime-content* mailmime-parameter*) 0)
          (abort "failed to append mailmime-parameter"))
        (let ((mailmime-clist* (mailmime-clist-new)))
          (unless mailmime-clist*
            (abort "failed to create clist"))
          (let ((mailmime*
                  (mailmime-new
                    mailmime-multiple #f 0
                    mailmime-fields* mailmime-content*
                    #f #f #f
                    mailmime-clist*
                    #f #f)))
            (unless mailmime*
              (abort "failed to create mailmime"))
            (unless (eq? (mailmime-add-part parent-mailmime* mailmime*) 0)
              (abort "failed to add mailmime part"))
            mailmime*))))))

;; sets the email fields of a mailmime*
(: mailmime*-set-email-fields ((struct email) pointer -> noreturn))
(define (mailmime*-set-email-fields email mailmime*)
  (let ((from* (mailimf-mailbox-list-new-empty)))
    (unless from*
      (abort "failed to create mailimf-mailbox-list"))
    (let ((add-parse-result (mailimf-mailbox-list-add-parse from* (email-from email))))
      (unless (eq? add-parse-result 0)
        (abort "failed to add parse mailimf-mailbox-list"))
      (let ((to* (mailimf-address-list-new-empty)))
        (unless to*
          (abort "failed to create mailimf-mailbox-list"))
        (let ((add-parse-result (mailimf-address-list-add-parse to* (email-to email))))
          (unless (eq? add-parse-result 0)
            (abort "failed to add parse mailimf-address-list"))
          (let ((mailimf-fields*
                  (mailimf-fields-new-with-data
                    from*
                    #f #f
                    to*
                    #f #f #f #f
                    (mailmime-strdup (email-subject email)))))
            (unless mailimf-fields*
              (abort "failed to create mailimf-fields"))
            (mailmime-set-imf-fields mailmime* mailimf-fields*)))))))
