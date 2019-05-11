(import (chicken condition))
(import (chicken format))

(declare (unit configuration))

(declare (uses config))
(declare (uses configuration-intern))
(declare (uses exceptions))

;; encapsulates a configuration node
(define-typed-record configuration-node
  (config-setting-t* pointer))

;; reads the configuration from a file and
;; invokes a procedure with the root section node
(: read-configuration-file (forall (r) (string ((struct configuration-node) -> r) -> r)))
(define (read-configuration-file file-name procedure)
  (with-guaranteed-release
    (lambda ()
      (malloc-config-t))
    (lambda (config-t*)
      (config-init config-t*)
      (if (eq? (config-read-file config-t* file-name) 0)
        (abort
          (format "failed to read configuration file ~A: ~A at line ~A"
            file-name
            (config-error-text config-t*)
            (config-error-line config-t*))))
      (procedure
        (make-configuration-node
          (config-root-setting config-t*))))
    (lambda (config-t*)
      (config-destroy config-t*)
      (free-config-t config-t*))))

;; returns a value from a section node
(: configuration-section-get-value ((struct configuration-node) string -> *))
(define (configuration-section-get-value configuration-node name)
  (let* ((config-setting-t* (configuration-node-config-setting-t* configuration-node))
         (member-config-setting-t* (config-setting-get-member config-setting-t* name)))
    (if member-config-setting-t*
      (get-value-from-config-setting-t* member-config-setting-t*)
      #f)))

;; returns a section node from a section node
(: configuration-section-get-section (
  (struct configuration-node) string -> (or (struct configuration-node) false)))
(define (configuration-section-get-section configuration-node name)
  (let* ((config-setting-t* (configuration-node-config-setting-t* configuration-node))
         (member-config-setting-t* (config-setting-get-member config-setting-t* name)))
    (if member-config-setting-t*
      (let ((config-type (config-setting-type member-config-setting-t*)))
        (if (eq? config-type config-type-group)
          (make-configuration-node member-config-setting-t*)
          #f))
      #f)))

;; returns a list node from a section node
(: configuration-section-get-list (
  (struct configuration-node) string -> (or (struct configuration-node) false)))
(define (configuration-section-get-list configuration-node name)
  (let* ((config-setting-t* (configuration-node-config-setting-t* configuration-node))
         (member-config-setting-t* (config-setting-get-member config-setting-t* name)))
    (if member-config-setting-t*
      (let ((config-type (config-setting-type member-config-setting-t*)))
        (if (or (eq? config-type config-type-array) (eq? config-type config-type-list))
          (make-configuration-node member-config-setting-t*)
          #f))
      #f)))

;; returns a value from a list node
(: configuration-list-get-value ((struct configuration-node) fixnum -> *))
(define (configuration-list-get-value configuration-node index)
  (let* ((config-setting-t* (configuration-node-config-setting-t* configuration-node))
         (element-config-setting-t* (config-setting-get-elem config-setting-t* index)))
    (if element-config-setting-t*
      (get-value-from-config-setting-t* element-config-setting-t*)
      #f)))

;; returns a section node from a list node
(: configuration-list-get-section (
  (struct configuration-node) fixnum -> (or (struct configuration-node) false)))
(define (configuration-list-get-section configuration-node index)
  (let* ((config-setting-t* (configuration-node-config-setting-t* configuration-node))
         (element-config-setting-t* (config-setting-get-elem config-setting-t* index)))
    (if element-config-setting-t*
      (let ((config-type (config-setting-type element-config-setting-t*)))
        (if (eq? config-type config-type-group)
          (make-configuration-node element-config-setting-t*)
          #f))
      #f)))

;; returns a list node from a list node
(: configuration-list-get-list (
  (struct configuration-node) fixnum -> (or (struct configuration-node) false)))
(define (configuration-list-get-list configuration-node index)
  (let* ((config-setting-t* (configuration-node-config-setting-t* configuration-node))
         (element-config-setting-t* (config-setting-get-elem config-setting-t* index)))
    (if element-config-setting-t*
      (let ((config-type (config-setting-type element-config-setting-t*)))
        (if (or (eq? config-type config-type-array) (eq? config-type config-type-list))
          (make-configuration-node element-config-setting-t*)
          #f))
      #f)))

;; returns the length of a list node
(: configuration-list-length ((struct configuration-node) -> fixnum))
(define (configuration-list-length configuration-node)
  (let ((config-setting-t* (configuration-node-config-setting-t* configuration-node)))
    (config-setting-length config-setting-t*)))
