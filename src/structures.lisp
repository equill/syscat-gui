;   Copyright 2020-2022 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

(in-package #:syscat-gui)

;;; Customised Hunchentoot acceptor.
;;; Carries information about the datastore being used.
(defclass syscat-gui-acceptor (tbnl:easy-acceptor)
  ;; Subclass attributes
  ((rg-server :initarg :rg-server
              :reader rg-server
              :initform (error ":rg-server is a required parameter."))
   (url-base :initarg :url-base
             :reader url-base
             :initform "localhost")
   (static-path :initarg :static-path
                :reader static-path))
  ;; Superclass defaults
  (:default-initargs :address "127.0.0.1")
  ;; Note to those asking.
  (:documentation "vhost object, subclassed from tbnl:easy-acceptor"))

(defun make-acceptor ()
  "Return an instance of 'syscat-gui-acceptor, a subclass of tbnl:easy-acceptor."
  (make-instance 'syscat-gui-acceptor
                 :address (or (sb-ext:posix-getenv "LISTEN_ADDR")
                              (getf *config-vars* :listen-address))
                 :port (or (when (sb-ext:posix-getenv "LISTEN_PORT")
                             (parse-integer (sb-ext:posix-getenv "LISTEN_PORT")))
                           (getf *config-vars* :listen-port))
                 :url-base (or (getf *config-vars* ::url-base) "")
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


(defclass schema-rtype-attrs ()
  ((name :initarg :name
         :reader name
         :type string
         :initform (error "name is mandatory"))
   (description :initarg :description
                :reader description
                :type (or null string)
                :initform nil)
   (readonly :initarg :readonly
           :reader readonly
           :type boolean
           :initform nil))
  (:documentation "Attributes of resource-types. Abstract type: *do not* instantiate directly; instead, use one of its subclasses."))

(defclass schema-rtype-attr-varchar (schema-rtype-attrs)
  ((maxlength :initarg :maxlength
              :reader maxlength
              :type (or null integer)
              :initform nil)
   (attrvalues :initarg :attrvalues
               :reader attrvalues
               :type (or null list)
               :initform nil))
  (:documentation "Variable-length string, for one-line values with optionally limited length. For bulk text, use schema-rtype-attr-text."))

(defclass schema-rtype-attr-text (schema-rtype-attrs)
  ()
  (:documentation "Block text, up to 65535 characters. For one-liners, see schema-rtype-attr-varchar."))

(defclass schema-rtype-attr-integer (schema-rtype-attrs)
  ((minimum :initarg :minimum
            :reader minimum
            :type (or null integer)
            :initform nil)
   (maximum :initarg :maximum
            :reader maximum
            :type (or null integer)
            :initform nil))
  (:documentation "Integer, with optional minimum and maximum values."))

(defclass schema-rtype-attr-boolean (schema-rtype-attrs)
  ()
  (:documentation "Boolean. Does what you'd expect."))


(defun make-schema-rtype-attrs (attribute)
  "Instantiate a subclass of schema-rtype-attrs according to the data provided.
   `attribute` is expected to be an alist, as fetched from the Restagraph schema endpoint."
  (cond
    ;; Text-block
    ((equal "text" (cdr (assoc :type attribute)))
     (make-instance 'schema-rtype-attr-text
                    :name (cdr (assoc :name attribute))
                    :description (or (cdr (assoc :description attribute)) "")
                    :readonly (cdr (assoc :readonly attribute))))
    ;; Integer
    ((equal "integer" (cdr (assoc :type attribute)))
     (make-instance 'schema-rtype-attr-integer
                    :name (cdr (assoc :name attribute))
                    :description (or (cdr (assoc :description attribute)) "")
                    :readonly (cdr (assoc :readonly attribute))
                    :minimum (cdr (assoc :minimum attribute))
                    :maximum (cdr (assoc :maximum attribute))))
    ;; Boolean
    ((equal "boolean" (cdr (assoc :type attribute)))
     (make-instance 'schema-rtype-attr-boolean
                    :name (cdr (assoc :name attribute))
                    :description (or (cdr (assoc :description attribute)) "")
                    :readonly (cdr (assoc :readonly attribute))))
    ;; Explicitly defined as a varchar
    ((equal "varchar" (cdr (assoc :type attribute)))
     (make-instance 'schema-rtype-attr-varchar
                    :name (cdr (assoc :name attribute))
                    :description (or (cdr (assoc :description attribute)) "")
                    :readonly (cdr (assoc :readonly attribute))
                    :maxlength (cdr (assoc :maxlength attribute))
                    :attrvalues (cdr (assoc :values attribute))))
    ;; Default to varchar, but without maxlength or attrvalues.
    (t
     (make-instance 'schema-rtype-attr-varchar
                    :name (cdr (assoc :name attribute))
                    :description (or (cdr (assoc :description attribute)) "")
                    :readonly (cdr (assoc :readonly attribute))))))


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
