(declare (unit b64))

(foreign-declare "

#include <b64/cencode.h>
#include <b64/cdecode.h>

// allocates a base64_encodestate
base64_encodestate* malloc_base64_encodestate()
{
  base64_encodestate* state = malloc(sizeof(base64_encodestate));
  return state;
}

// frees a base64_encodestate
void free_base64_encodestate(base64_encodestate* state)
{
  free(state);
}

// allocates a base64_decodestate
base64_decodestate* malloc_base64_decodestate()
{
  base64_decodestate* state = malloc(sizeof(base64_decodestate));
  return state;
}

// frees a base64_decodestate
void free_base64_decodestate(base64_decodestate* state)
{
  free(state);
}

// wraps the base64_encode_block function
char* base64_encode_block_wrapped(
  unsigned char* plaintext_in, int length_in,
  base64_encodestate* state_in)
{
  char* encoded = malloc(((length_in * 2) + 16) * sizeof(char));
  if (encoded == NULL)
  {
    return NULL;
  }

  char* encoded_to = encoded + base64_encode_block((char*)plaintext_in, length_in, encoded, state_in);
  encoded_to += base64_encode_blockend(encoded_to, state_in);

  *encoded_to = 0;

  return encoded;
}

// wraps the base64_decode_block function
int base64_decode_block_wrapped(
  unsigned char* code_in, int length_in,
  unsigned char* plaintext_out,
  base64_decodestate* state_in)
{
  return base64_decode_block((char*)code_in, length_in, (char*)plaintext_out, state_in);
}

")

;; base64-encodestate pointers definitions
(define-foreign-type base64-encodestate "base64_encodestate")
(define-foreign-type base64-encodestate* (c-pointer base64-encodestate))

;; base64-encodestate pointers memory management
(define malloc-base64-encodestate (foreign-lambda base64-encodestate* "malloc_base64_encodestate"))
(define free-base64-encodestate (foreign-lambda void "free_base64_encodestate" base64-encodestate*))

;; base64-decodestate pointers definitions
(define-foreign-type base64-decodestate "base64_decodestate")
(define-foreign-type base64-decodestate* (c-pointer base64-decodestate))

;; base64-decodestate pointers memory management
(define malloc-base64-decodestate (foreign-lambda base64-decodestate* "malloc_base64_decodestate"))
(define free-base64-decodestate (foreign-lambda void "free_base64_decodestate" base64-decodestate*))

;; initializes the states
(define base64-init-encodestate (foreign-lambda void "base64_init_encodestate" base64-encodestate*))
(define base64-init-decodestate (foreign-lambda void "base64_init_decodestate" base64-decodestate*))

;; encodes a block
(define base64-encode-block
  (foreign-lambda c-string* "base64_encode_block_wrapped"
    u8vector int base64-encodestate*))

;; decodes a block
(define base64-decode-block
  (foreign-lambda int "base64_decode_block_wrapped"
    u8vector int u8vector base64-decodestate*))
