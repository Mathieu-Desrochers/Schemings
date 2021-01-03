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

;; sets the email fields of a mailmime*
(: mailmime*-set-email-fields ((struct email) pointer -> noreturn))
(define (mailmime*-set-email-fields email mailmime*)
  (let ((from* (mailimf-mailbox-list-new-empty))
        (reply-to* (if (email-reply-to email) (mailimf-address-list-new-empty) #f))
        (to* (mailimf-address-list-new-empty)))
    (unless from* (abort "failed to create mailimf-mailbox-list"))
    (if (email-reply-to email) (unless reply-to* (abort "failed to create mailimf-mailbox-list")))
    (unless to* (abort "failed to create mailimf-mailbox-list"))
    (unless (eq? (mailimf-mailbox-list-add-parse from* (email-from email)) 0)
      (abort "failed to add parse mailimf-mailbox-list"))
    (if (email-reply-to email)
      (unless (eq? (mailimf-address-list-add-parse reply-to* (email-reply-to email)) 0)
        (abort "failed to add parse mailimf-address-list")))
    (unless (eq? (mailimf-address-list-add-parse to* (email-to email)) 0)
      (abort "failed to add parse mailimf-address-list"))
    (let ((mailimf-fields*
            (mailimf-fields-new-with-data
              from*
              #f
              reply-to*
              to*
              #f #f #f #f
              (mailmime-strdup (email-subject email)))))
      (unless mailimf-fields*
        (abort "failed to create mailimf-fields"))
      (mailmime-set-imf-fields mailmime* mailimf-fields*))))

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
(: add-text-mailmime* (pointer string string -> noreturn))
(define (add-text-mailmime* parent-mailmime* content-type text)
  (let* ((content (string->blob text))
         (mailmime*
            (make-single-mailmime*
              content-type
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
(: add-multiple-mailmime* (pointer string string -> pointer))
(define (add-multiple-mailmime* parent-mailmime* multipart-type boundary)
  (let ((mailmime-fields*
          (mailmime-fields-new-empty)))
    (unless mailmime-fields*
      (abort "failed to create mailmime-fields"))
    (let ((mailmime-content*
            (mailmime-content-new-with-str multipart-type)))
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
