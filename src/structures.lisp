;   Copyright 2020 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

(in-package #:cl-webcat)

;;; Customised Hunchentoot acceptor.
;;; Carries information about the datastore being used.
(defclass cl-webcat-acceptor (tbnl:easy-acceptor)
  ;; Subclass attributes
  ((rg-server :initarg :rg-server
              :reader rg-server
              :initform (make-acceptor))
   (url-base :initarg :url-base
             :reader url-base
             :initform "localhost")
   (template-path :initarg :template-path
                  :reader template-path)
   (static-path :initarg :static-path
                :reader static-path))
  ;; Superclass defaults
  (:default-initargs :address "127.0.0.1")
  ;; Note to those asking.
  (:documentation "vhost object, subclassed from tbnl:easy-acceptor"))

(defun make-acceptor ()
  "Return an instance of 'cl-webcat-acceptor, a subclass of tbnl:easy-acceptor."
  (make-instance 'cl-webcat-acceptor
                 :address (or (sb-ext:posix-getenv "LISTEN_ADDR")
                              (getf *config-vars* :listen-address))
                 :port (or (when (sb-ext:posix-getenv "LISTEN_PORT")
                             (parse-integer (sb-ext:posix-getenv "LISTEN_PORT")))
                           (getf *config-vars* :listen-port))
                 :url-base (or (getf *config-vars* ::url-base) "")
                 :template-path (make-pathname :defaults
                                               (or (sb-ext:posix-getenv "TEMPLATE_PATH")
                                                   (getf *config-vars* :template-path)))
                 :static-path (make-pathname :defaults
                                             (or (sb-ext:posix-getenv "STATIC_PATH")
                                                 (getf *config-vars* :static-path)))
                 ;; Send all logs to STDOUT, and let Docker sort 'em out
                 :access-log-destination (make-synonym-stream 'cl:*standard-output*)
                 :message-log-destination (make-synonym-stream 'cl:*standard-output*)
                 ;; Restagraph connection details
                 :rg-server (make-rg-server
                              :hostname (or (sb-ext:posix-getenv "RG_HOSTNAME")
                                            (getf *config-vars* :rg-hostname))
                              :port (or (when (sb-ext:posix-getenv "RG_PORT")
                                          (parse-integer (sb-ext:posix-getenv "RG_PORT")))
                                        (getf *config-vars* :rg-port))
                              :raw-base (or (sb-ext:posix-getenv "RG_RAW_BASE")
                                            (getf *config-vars* :api-uri-base))
                              :files-base (or (sb-ext:posix-getenv "RG_FILES_BASE")
                                              (getf *config-vars* :files-uri-base))
                              :schema-base (or (sb-ext:posix-getenv "RG_SCHEMA_BASE")
                                               (getf *config-vars* :schema-uri-base)))))

(defstruct rg-server
  "The details needed to connect to the backend Restagraph server."
  (hostname nil :type string :read-only t)
  (port nil :type integer :read-only t)
  (raw-base nil :type string :read-only t)
  (files-base nil :type string :read-only t)
  (schema-base nil :type string :read-only t))

(defstruct schema-rtype-attrs
  "Attributes of resource-types. Copy-pasted straight from restagraph/src/structures.lisp."
  (name nil :type string :read-only t)
  (description "" :type string :read-only t)
  (values nil :type list :read-only t)
  ;; The type of value. To be used for validation, and for rendering of the value.
  (valuetype "text" :type string :read-only t))


(defclass outbound-rels ()
  ((name :initarg :name
                 :reader name
                 :initform (error "Required argument."))
   (target-type :initarg :target-type
                 :reader target-type
                 :initform (error "Required argument."))
   (dependent-p :initarg :dependent-p
                :reader dependent-p
                :initform nil))
  (:documentation "Representation of outbound relationships from a resource."))

(defclass linked-resource ()
  ((relationship :initarg :relationship
                 :reader relationship
                 :initform (error "Required argument."))
   (target-type :initarg :target-type
                :reader target-type
                :initform (error "Required argument."))
   (dependent-p :initarg :dependent-p
                :reader dependent-p
                :initform nil)
   (uid :initarg :uid
        :reader uid
        :initform (error "Required argument")))
  (:documentation "Description of a resource linked from another resource"))
