(declare (unit etpan))

(foreign-declare "

#include <stdlib.h>
#include <string.h>
#include <libetpan/libetpan.h>

// wraps the clist_append function
// for mailmime_content ct_parameters
int mailmime_content_add_parameter(
  struct mailmime_content* mailmime_content,
  struct mailmime_parameter* mailmime_parameter)
{
  return clist_append(
    mailmime_content->ct_parameters,
    mailmime_parameter);
}

// wraps the mailmime_write_file function
char* mailmime_write_file_wrapped(struct mailmime* mailmime)
{
  char* buffer = NULL;
  size_t buffer_len = 0;

  FILE* file = open_memstream(&buffer, &buffer_len);
  if (file == NULL)
  {
    return NULL;
  }

  int col = 0;

  int mailmime_write_file_result = mailmime_write_file(file, &col, mailmime);
  if (mailmime_write_file_result != 0)
  {
    fflush(file);
    fclose(file);
    free(buffer);

    return NULL;
  }

  fflush(file);
  fclose(file);

  return buffer;
}

")

;; mailmime codes
(define mailmime-mechanism-base64 (foreign-value "MAILMIME_MECHANISM_BASE64" int))
(define mailmime-disposition-type-inline (foreign-value "MAILMIME_DISPOSITION_TYPE_INLINE" int))
(define mailmime-disposition-type-attachment (foreign-value "MAILMIME_DISPOSITION_TYPE_ATTACHMENT" int))
(define mailmime-single (foreign-value "MAILMIME_SINGLE" int))
(define mailmime-multiple (foreign-value "MAILMIME_MULTIPLE" int))

;; mailmime clist pointers definitions
(define-foreign-type mailmime-clist "clist")
(define-foreign-type mailmime-clist* (c-pointer mailmime-clist))

;; mailmime mechanism pointers definitions
(define-foreign-type mailmime-mechanism "struct mailmime_mechanism")
(define-foreign-type mailmime-mechanism* (c-pointer mailmime-mechanism))

;; mailmime disposition pointers definitions
(define-foreign-type mailmime-disposition "struct mailmime_disposition")
(define-foreign-type mailmime-disposition* (c-pointer mailmime-disposition))

;; mailmime fields pointers definitions
(define-foreign-type mailmime-fields "struct mailmime_fields")
(define-foreign-type mailmime-fields* (c-pointer mailmime-fields))

;; mailmime parameter pointers definitions
(define-foreign-type mailmime-parameter "struct mailmime_parameter")
(define-foreign-type mailmime-parameter* (c-pointer mailmime-parameter))

;; mailmime content pointers definitions
(define-foreign-type mailmime-content "struct mailmime_content")
(define-foreign-type mailmime-content* (c-pointer mailmime-content))

;; mailmime pointers definitions
(define-foreign-type mailmime "struct mailmime")
(define-foreign-type mailmime* (c-pointer mailmime))

;; mailmime utility functions
(define mailmime-clist-new (foreign-lambda mailmime-clist* "clist_new"))
(define mailmime-strdup (foreign-lambda c-pointer "strdup" c-string))

;; creates and initializes a mailmime mechanism
(define mailmime-mechanism-new
  (foreign-lambda mailmime-mechanism* "mailmime_mechanism_new" int c-string))

;; creates and initializes a mailmime disposition
(define mailmime-disposition-new-with-data
  (foreign-lambda mailmime-disposition* "mailmime_disposition_new_with_data"
    int c-pointer c-string c-string c-string int))

;; creates and initializes mailmime fields
(define mailmime-fields-new-empty
  (foreign-lambda mailmime-fields* "mailmime_fields_new_empty"))
(define mailmime-fields-new-with-data
  (foreign-lambda mailmime-fields* "mailmime_fields_new_with_data"
    mailmime-mechanism* c-string c-string mailmime-disposition* c-pointer))

;; creates and initializes a mailmime parameter
(define mailmime-param-new-with-data
  (foreign-lambda mailmime-parameter* "mailmime_param_new_with_data" c-string c-string))

;; creates and initializes a mailmime content
(define mailmime-content-new-with-str
  (foreign-lambda mailmime-content* "mailmime_content_new_with_str" c-string))
(define mailmime-content-add-parameter
  (foreign-lambda int "mailmime_content_add_parameter" mailmime-content* mailmime-parameter*))

;; creates and initializes a mailmime
(define mailmime-new
  (foreign-lambda mailmime* "mailmime_new"
    int c-pointer int mailmime-fields* mailmime-content* c-pointer
    c-pointer c-pointer mailmime-clist* c-pointer c-pointer))
(define mailmime-new-message-data
  (foreign-lambda mailmime* "mailmime_new_message_data" mailmime*))

;; builds a mailmime
(define mailmime-set-body-text
  (foreign-lambda int "mailmime_set_body_text" mailmime* u8vector int))
(define mailmime-add-part
  (foreign-lambda int "mailmime_add_part" mailmime* mailmime*))

;; converts a mailmime to string
(define mailmime-write-file
  (foreign-lambda c-string* "mailmime_write_file_wrapped" mailmime*))

;; frees a mailmime
(define mailmime-free (foreign-lambda void "mailmime_free" mailmime*))

;; mailimf mailbox list pointers definitions
(define-foreign-type mailimf-mailbox-list "struct mailimf_mailbox_list")
(define-foreign-type mailimf-mailbox-list* (c-pointer mailimf-mailbox-list))

;; mailimf address list pointers definitions
(define-foreign-type mailimf-address-list "struct mailimf_address_list")
(define-foreign-type mailimf-address-list* (c-pointer mailimf-address-list))

;; mailimf fields pointers definitions
(define-foreign-type mailimf-fields "struct mailimf_fields")
(define-foreign-type mailimf-fields* (c-pointer mailimf-fields))

;; creates a new empty list of mailboxes
(define mailimf-mailbox-list-new-empty
  (foreign-lambda mailimf-mailbox-list* "mailimf_mailbox_list_new_empty"))

;; adds a mailbox given in form of a string
(define mailimf-mailbox-list-add-parse
  (foreign-lambda int "mailimf_mailbox_list_add_parse" mailimf-mailbox-list* c-string))

;; creates a new empty list of addresses
(define mailimf-address-list-new-empty
  (foreign-lambda mailimf-address-list* "mailimf_address_list_new_empty"))

;; adds an address given in form of a string
(define mailimf-address-list-add-parse
  (foreign-lambda int "mailimf_address_list_add_parse" mailimf-address-list* c-string))

;; creates a set of headers with some headers
(define mailimf-fields-new-with-data
  (foreign-lambda mailimf-mailbox-list* "mailimf_fields_new_with_data"
    mailimf-mailbox-list* c-pointer c-pointer c-pointer c-pointer
    c-pointer c-pointer c-pointer c-pointer))

;; set the fields of the given mime message
(define mailmime-set-imf-fields
  (foreign-lambda void "mailmime_set_imf_fields" mailmime* mailimf-fields*))

;; mailsmtp codes
(define mailsmtp-no-error (foreign-value "MAILSMTP_NO_ERROR" int))

;; mailsmtp pointers definitions
(define-foreign-type mailsmtp "mailsmtp")
(define-foreign-type mailsmtp* (c-pointer mailsmtp))

;; progress_function pointer definitions
(define-foreign-type progress-function "progress_function")
(define-foreign-type progress-function* (c-pointer progress-function))

;; opens a smtp connection
(define mailsmtp-new
  (foreign-lambda mailsmtp* "mailsmtp_new" int progress-function*))
(define mailsmtp-socket-connect
  (foreign-lambda int "mailsmtp_socket_connect" mailsmtp* (const c-string) short))
(define mailesmtp-ehlo
  (foreign-lambda int "mailesmtp_ehlo" mailsmtp*))
(define mailsmtp-socket-starttls
  (foreign-lambda int "mailsmtp_socket_starttls" mailsmtp*))
(define mailsmtp-auth
  (foreign-lambda int "mailsmtp_auth" mailsmtp* (const c-string) (const c-string)))

;; sends an email
(define mailsmtp-mail
  (foreign-lambda int "mailsmtp_mail" mailsmtp* (const c-string)))
(define mailsmtp-rcpt
  (foreign-lambda int "mailsmtp_rcpt" mailsmtp* (const c-string)))
(define mailsmtp-data
  (foreign-lambda int "mailsmtp_data" mailsmtp*))
(define mailsmtp-data-message
  (foreign-lambda int "mailsmtp_data_message" mailsmtp* (const c-string) int))

;; closes a smtp connection
(define mailsmtp-free (foreign-lambda void "mailsmtp_free" mailsmtp*))
