(declare (unit config))

(foreign-declare "

#include <libconfig.h>

// allocates a config_t
config_t* malloc_config_t()
{
  config_t* config = malloc(sizeof(config_t));
  return config;
}

// frees a config_t
void free_config_t(config_t* config)
{
  free(config);
}

")

;; config-t pointers definitions
(define-foreign-type config-t "config_t")
(define-foreign-type config-t* (c-pointer config-t))

;; config-t pointers memory management
(define malloc-config-t (foreign-lambda config-t* "malloc_config_t"))
(define free-config-t (foreign-lambda void "free_config_t" config-t*))

;; config-setting-t pointers definitions
(define-foreign-type config-setting-t "config_setting_t")
(define-foreign-type config-setting-t* (c-pointer config-setting-t))

;; initialize the configuration object
(define config-init (foreign-lambda void "config_init" config-t*))

;; reads and parses a configuration from the file
(define config-read-file (foreign-lambda int "config_read_file" config-t* (const c-string)))
(define config-error-text (foreign-lambda c-string "config_error_text" config-t*))
(define config-error-line (foreign-lambda int "config_error_line" config-t*))

;; returns the root setting for the configuration
(define config-root-setting (foreign-lambda config-setting-t* "config_root_setting" config-t*))

;; returns the type of the given setting
(define config-type-int (foreign-value "CONFIG_TYPE_INT" int))
(define config-type-float (foreign-value "CONFIG_TYPE_FLOAT" int))
(define config-type-string (foreign-value "CONFIG_TYPE_STRING" int))
(define config-type-bool (foreign-value "CONFIG_TYPE_BOOL" int))
(define config-type-array (foreign-value "CONFIG_TYPE_ARRAY" int))
(define config-type-list (foreign-value "CONFIG_TYPE_LIST" int))
(define config-type-group (foreign-value "CONFIG_TYPE_GROUP" int))
(define config-setting-type (foreign-lambda int "config_setting_type" config-setting-t*))

;; return the value of the given setting
(define config-setting-get-int (foreign-lambda int "config_setting_get_int" config-setting-t*))
(define config-setting-get-float (foreign-lambda double "config_setting_get_float" config-setting-t*))
(define config-setting-get-bool (foreign-lambda int "config_setting_get_bool" config-setting-t*))
(define config-setting-get-string (foreign-lambda c-string "config_setting_get_string" config-setting-t*))

;; fetches the child setting named name from the group setting
(define config-setting-get-member
  (foreign-lambda config-setting-t* "config_setting_get_member"
    config-setting-t* (const c-string)))

;; returns the number of elements in a list or array
(define config-setting-length (foreign-lambda int "config_setting_length" config-setting-t*))

;; fetches the element at the given index
(define config-setting-get-elem
  (foreign-lambda config-setting-t* "config_setting_get_elem"
    config-setting-t* int))

;; destroy the configuration object
(define config-destroy (foreign-lambda void "config_destroy" config-t*))
