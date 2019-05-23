(import (chicken condition))
(import (chicken format))

(declare (unit smtp))

(declare (uses etpan))
(declare (uses exceptions))

;; encapsulates a smtp connection
(define-typed-record smtp-connection
  (server string)
  (port fixnum)
  (user string)
  (password string)
  (mailsmtp* pointer))

;; invokes a procedure with a smtp connection
(: with-smtp-connection
  (forall (r) (string fixnum string string ((struct smtp-connection) -> r) -> r)))
(define (with-smtp-connection server port user password procedure)
  (with-guaranteed-release
    (lambda ()
      (let ((mailsmtp* (mailsmtp-new 0 #f)))
        (unless mailsmtp*
          (abort "failed to allocate mailsmtp*"))
        mailsmtp*))
    (lambda (mailsmtp*)
      (let ((smtp-connection (make-smtp-connection server port user password mailsmtp*)))
        (procedure smtp-connection)))
    mailsmtp-free))

;; sends an email
(: smtp-send ((struct smtp-connection) (struct email) -> noreturn))
(define (smtp-send smtp-connection email)

  ;; connect
  (let ((mailsmtp-socket-connect-result
          (mailsmtp-socket-connect
            (smtp-connection-mailsmtp* smtp-connection)
            (smtp-connection-server smtp-connection)
            (smtp-connection-port smtp-connection))))
    (unless (= mailsmtp-socket-connect-result mailsmtp-no-error)
      (abort
        (format "failed to connect to smtp server ~A:~A with error code ~A"
          (smtp-connection-server smtp-connection)
          (smtp-connection-port smtp-connection)
          mailsmtp-socket-connect-result))))

  ;; say helo
  (let ((mailesmtp-ehlo-result
          (mailesmtp-ehlo
            (smtp-connection-mailsmtp* smtp-connection))))
    (unless (= mailesmtp-ehlo-result mailsmtp-no-error)
      (abort
        (format "failed to elho smtp server ~A:~A with error code ~A"
          (smtp-connection-server smtp-connection)
          (smtp-connection-port smtp-connection)
          mailesmtp-ehlo-result))))

  ;; start tls
  (let ((mailsmtp-socket-starttls-result
          (mailsmtp-socket-starttls
            (smtp-connection-mailsmtp* smtp-connection))))
    (unless (= mailsmtp-socket-starttls-result mailsmtp-no-error)
      (abort
        (format "failed to starttls smtp server ~A:~A with error code ~A"
          (smtp-connection-server smtp-connection)
          (smtp-connection-port smtp-connection)
          mailsmtp-socket-starttls-result))))

  ;; say helo again
  (let ((mailesmtp-ehlo-again-result
          (mailesmtp-ehlo
            (smtp-connection-mailsmtp* smtp-connection))))
    (unless (= mailesmtp-ehlo-again-result mailsmtp-no-error)
      (abort
        (format "failed to elho again smtp server ~A:~A with error code ~A"
          (smtp-connection-server smtp-connection)
          (smtp-connection-port smtp-connection)
          mailesmtp-ehlo-again-result))))

  ;; authenticate
  (let ((mailsmtp-auth-result
          (mailsmtp-auth
            (smtp-connection-mailsmtp* smtp-connection)
            (smtp-connection-user smtp-connection)
            (smtp-connection-password smtp-connection))))
    (unless (= mailsmtp-auth-result mailsmtp-no-error)
      (abort
        (format "failed to auth to smtp server ~A:~A with error code ~A"
          (smtp-connection-server smtp-connection)
          (smtp-connection-port smtp-connection)
          mailsmtp-auth-result))))

  ;; prepare the origin
  (let ((mailsmtp-mail-result
          (mailsmtp-mail
            (smtp-connection-mailsmtp* smtp-connection)
            (email-from email))))
    (unless (= mailsmtp-mail-result mailsmtp-no-error)
      (abort
        (format "failed to prepare email from ~A with error code ~A"
          (email-from email)
          mailsmtp-mail-result))))

  ;; prepare the recepient
  (let ((mailsmtp-rcpt-result
          (mailsmtp-rcpt
            (smtp-connection-mailsmtp* smtp-connection)
            (email-to email))))
    (unless (= mailsmtp-rcpt-result mailsmtp-no-error)
      (abort
        (format "failed to add recipient ~A with error code ~A"
          (email-to email)
          mailsmtp-rcpt-result))))

  ;; prepare the data
  (let ((mailsmtp-data-result
          (mailsmtp-data
            (smtp-connection-mailsmtp* smtp-connection))))
    (unless (= mailsmtp-data-result mailsmtp-no-error)
      (abort
        (format "failed to prepare data with error code ~A"
          mailsmtp-data-result))))

  ;; send the email
  (let* ((email-content (email->string email))
         (mailsmtp-data-message-result
            (mailsmtp-data-message
              (smtp-connection-mailsmtp* smtp-connection)
              email-content
              (string-length email-content))))
    (unless (= mailsmtp-data-message-result mailsmtp-no-error)
      (abort
        (format "failed to set message of length ~A with error code ~A"
          (string-length email-content)
          mailsmtp-data-message-result)))))
