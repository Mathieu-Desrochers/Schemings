(declare (unit zeromq))

(foreign-declare "

#include <zmq.h>

// sets a socket option having an int value
int zmq_setsockopt_int(void* socket, int option, int value) {
  return zmq_setsockopt(socket, option, &value, sizeof(value));
}

")

;; zmq-ctx pointer definitions
(define-foreign-type zmq-ctx* (c-pointer void))

;; zmq-socket pointer definitions
(define-foreign-type zmq-socket* (c-pointer void))

;; constants
(define zmq-dontwait (foreign-value "ZMQ_DONTWAIT" int))
(define zmq-linger (foreign-value "ZMQ_LINGER" int))
(define zmq-pull (foreign-value "ZMQ_PULL" int))
(define zmq-push (foreign-value "ZMQ_PUSH" int))
(define zmq-sndtimeo (foreign-value "ZMQ_SNDTIMEO" int))

;; create new zmq-context
(define zmq-ctx-new (foreign-lambda zmq-ctx* "zmq_ctx_new"))
(define zmq-ctx-destroy (foreign-lambda int "zmq_ctx_destroy" zmq-ctx*))

;; create zmq-socket
(define zmq-socket (foreign-lambda zmq-socket* "zmq_socket" zmq-ctx* int))
(define zmq-close (foreign-lambda int "zmq_close" zmq-socket*))

;; set socket option
(define zmq-setsockopt-int (foreign-lambda int "zmq_setsockopt_int" zmq-socket* int int))

;; accept incoming connections on a socket
(define zmq-bind (foreign-lambda int "zmq_bind" zmq-socket* c-string))

;; create outgoing connection from socket
(define zmq-connect (foreign-lambda int "zmq_connect" zmq-socket* c-string))

;; send a message part on a socket
(define zmq-send (foreign-lambda int "zmq_send" zmq-socket* u8vector int int))

;; receive a message part from a socket
(define zmq-recv (foreign-lambda int "zmq_recv" zmq-socket* u8vector int int))
