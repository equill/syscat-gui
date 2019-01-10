;   Copyright 2019 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; The REST API server application

(in-package #:cl-webcat)

;;; Customised Hunchentoot acceptor.
;;; Carries information about the datastore being used.
(defclass cl-webcat-acceptor (tbnl:easy-acceptor)
  ;; Subclass attributes
  ((rg-server :initarg :rg-server
            :reader rg-server
            :initform (make-default-acceptor))
   (url-base :initarg :url-base
             :reader url-base
             :initform "localhost"))
  ;; Superclass defaults
  (:default-initargs :address "127.0.0.1")
  ;; Note to those asking.
  (:documentation "vhost object, subclassed from tbnl:easy-acceptor"))

;;; Define a logging method
(defmethod tbnl:acceptor-log-message ((acceptor cl-webcat-acceptor)
                                      log-level
                                      format-string
                                      &rest format-arguments)
  (log-message log-level (append (list format-string) format-arguments)))


(defstruct rg-server
  "The details needed to connect to the backend Restagraph server."
  (hostname nil :type string :read-only t)
  (port nil :type integer :read-only t)
  (raw-base nil :type string :read-only t)
  (schema-base nil :type string :read-only t))


;;; Utility functions

(defun get-uri-parts (uri)
  "Break the URI into parts for processing by uri-node-helper.
  Expects a string; returns a list of strings."
  (mapcar
    #'sanitise-uid
    (cdr
        (ppcre:split "/"
                     (cl-ppcre:regex-replace (getf *config-vars* :api-uri-base) uri "")))))

(defun escape-neo4j (str)
  "Escape any undesirable characters in a string, e.g. the single-quote.
  Expects a string, and returns another string."
  (cl-ppcre:regex-replace-all
    "'"
    str
    "´"))


;;; Data retrieval functions

(defun decode-json-response (json)
  "Parse the JSON returned as application/json into a CL structure"
  ;; Neo4j sends a stream of octets. Convert this into a string.
  (let ((json-string (flexi-streams:octets-to-string json)))
    ;; If an empty string was returned, pass an empty string back.
    (if (equal json-string "")
        ""
        ;; If we received actual content, on the other hand, decode it.
        (cl-json:decode-json-from-string json-string))))

(defun rg-request-json (server uri &key schema-p)
  "Make a request to a Restagraph backend that should return a JSON response.
   Return the decoded JSON."
  (decode-json-response
    (drakma:http-request (format nil "http://~A:~D~A~A"
                                 (rg-server-hostname server)
                                 (rg-server-port server)
                                 (if schema-p
                                     (rg-server-schema-base server)
                                     (rg-server-raw-base server))
                                 uri))))

(defun get-uids (server resourcetype)
  "Retrieve a list of UIDs for a specified resourcetype.
   Arguments:
   - server = instance of rg-server struct
   - resourcetype = string
   Returns a list of strings."
  (mapcar #'(lambda (item)
              (cdr (assoc :uid item)))
          (rg-request-json server (format nil "/~A" resourcetype))))

(defun get-schema (server &optional (resourcetype ""))
  (rg-request-json server (format nil "/~A" resourcetype) :schema-p t))

(defun get-attrs (server resourcetype)
  "Retrieve a list of attributes for a resourcetype.
   Arguments:
   - server = instance of rg-server struct
   - resourcetype = string
   Returns a list of strings."
  (cdr (assoc :attributes
              (get-schema server resourcetype))))


;; Request handlers

(defun root ()
  "Default handler for requests to /"
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-ok+)
  "OK")

(defun healthcheck ()
  "Basic is-it-running endpoint"
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-ok+)
  "OK")


;; Error response functions

(defun four-oh-four ()
  "Fallthrough handler, for anything we haven't already defined."
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-not-found+)
  "This is not a valid URI")

(defun method-not-allowed ()
  "Default response for a client making a request we don't support"
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-method-not-allowed+)
  "Method not allowed")

(defun uri-not-implemented ()
  "It's an API request, but not one we're configured for."
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-not-implemented+)
  "Not implemented")

(defun return-integrity-error (logmessage &optional client-message)
  "Report to the client that their request would have violated an integrity constraint.
  The optional client-message "
  (log-message :warn (format nil "Client triggered integrity error: ~A" logmessage))
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-conflict+)
  ;; If we were handed a specific message, use that.
  ;; Otherwise, just pass on the logmessage.
  (if client-message client-message logmessage))

(defun return-database-error (message)
  "There was a database problem. Log it and report something generic to the user, not to keep them in the dark but to reduce the potential for data leakage."
  (log-message :error (format nil "Database error: ~A" message))
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
  "An error occurred with the database. This has been logged, and will be fixed.")

(defun return-transient-error (message)
  "Transient problem, which may already have self-resolved.. Log it and report something generic to the user, not to keep them in the dark but to reduce the potential for data leakage."
  (log-message :error (format nil "Database error: ~A" message))
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-service-unavailable+)
  "A transient error occurred, and has been logged for us to work on. Please try your request again.")

(defun return-client-error (logmessage &optional message)
  "The client made a bad request. Return this information to them, that they may learn from their mistakes."
  (log-message :info (format nil "Client error: ~A" logmessage))
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-bad-request+)
  ;; If we were handed a specific message, use that.
  ;; Otherwise, just pass on the logmessage.
  (format nil "Client error: ~A"
          (or message logmessage)))

(defun return-service-error (logmessage &optional message)
  "There was a problem with connecting to the backend service."
  (log-message :crit (format nil "Service error: ~A" logmessage))
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
  (format nil "Service error: ~A"
          (or message logmessage)))


;; Functions for dispatching requests

(defun sanitise-uid (uid)
  "Replace UID-unfriendly characters in UIDs with something safe.
  Expects a string and returns another string."
  (escape-neo4j
    (cl-ppcre:regex-replace-all "[/ ]" uid "_")))

(defun get-sub-uri (uri base-uri)
  "Extract the URI from the full request string,
   excluding the base URL and any GET parameters.
   Expects two strings; returns one string."
  (first (cl-ppcre:split "\\?" (cl-ppcre:regex-replace base-uri uri ""))))


;; Dispatchers

#+(or)
(defun web-dispatcher-v1 ()
  "Hunchentoot dispatch function for the web UI."
  (handler-case
    (let* ((uri-parts (get-uri-parts (tbnl:request-uri*)))
           (resourcetype (first uri-parts)))
      (cond
        ;;
        ;; Intercept and reject attempts to interact with the "any" resource-type
        ((equal (third uri-parts) "any")
         (progn
           (setf (tbnl:content-type*) "text/plain")
           (setf (tbnl:return-code*) tbnl:+http-not-found+)
           (format nil "No resources found for ~A" uri-parts)))
        ;;
        ;; GET -> Retrieve something
        ((equal (tbnl:request-method*) :GET)
         (log-message :debug
                      (format nil "Dispatching GET request for URI ~A"
                              (tbnl:request-uri*)))
         (let* (;; Extract the URI by dropping the base URL.
                ;; Do it separately because we use it again later in this function.
                (sub-uri (get-sub-uri (tbnl:request-uri*) (getf *config-vars* :api-uri-base)))
                ;; Get the search result
                (result (get-resources (datastore tbnl:*acceptor*)
                                       sub-uri
                                       :directional (when (tbnl:get-parameter "directional") t)
                                       :filters (remove-if
                                                  #'(lambda (par)
                                                      (equal (car par) "directional"))
                                                  (tbnl:get-parameters*)))))
           ;; Return what we found
           (progn
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-ok+)
             (cond
               ;; Single resource was requested, and nothing was found.
               ((and (= (mod (length uri-parts) 3) 2)
                     (or (null result)
                         (equal result "")))
                "[]")
               ;; Single resource was requested, and something was found.
               ((= (mod (length uri-parts) 3) 2)
                (cl-json:encode-json-alist-to-string result))
               ;; Class of resources was requested, and nothing was found.
               ((or (null result)
                    (equal result ""))
                "[]")
               ;; Class of resources was requested, and something was found.
               (t
                 (cl-json:encode-json-to-string result))))))
        ;; POST -> Store something
        ;;
        ;; Resource
        ((and
           (equal (tbnl:request-method*) :POST)
           (equal (length uri-parts) 1)
           (tbnl:post-parameter "uid"))
         (log-message :debug
                      (format nil
                              "Attempting to dispatch a POST request for resource type ~A"
                              resourcetype))
         ;; Do we already have one of these?
         (if (get-resources
               (datastore tbnl:*acceptor*)
               (format nil "/~A/~A" (car uri-parts) (tbnl:post-parameter "uid")))
           ;; It's already there; return 200/OK
           (progn
             (log-message
               :debug
               (format nil
                       "Doomed attempt to re-create resource /~A/~A. Reassuring the client that it's already there."
                       (car uri-parts)
                       (tbnl:post-parameter "uid")))
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-ok+)
             "Resource exists")
           ;; We don't already have one of these; store it
           (handler-case
             (progn
               (store-resource (datastore tbnl:*acceptor*)
                               resourcetype
                               (tbnl:post-parameters*))
               ;; Return it from the database, for confirmation
               (log-message :debug "Stored the new resource. Now retrieving it from the database, to return to the client.")
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-created+)
               (cl-json:encode-json-alist-to-string
                 (get-resources (datastore tbnl:*acceptor*)
                                (format nil "/~A/~A"
                                        resourcetype
                                        (tbnl:post-parameter "uid")))))
             ;; Handle integrity errors
             (cl-webcat:integrity-error (e) (return-integrity-error (message e)))
             ;; Handle general client errors
             (cl-webcat:client-error (e) (return-client-error (message e))))))
        ;;
        ;; Store a relationship
        ((and
           (equal (tbnl:request-method*) :POST)
           (equal (mod (length uri-parts) 3) 0)
           (tbnl:post-parameter "target"))
         ;; Grab these once, as we'll be referring to them a few times
         (let ((dest-path (tbnl:post-parameter "target")))
           ;; Basic sanity check
           (log-message :debug (format nil
                                       "Creating a relationship from ~A to ~A"
                                       (tbnl:request-uri*) dest-path))
           (handler-case
             (progn
               (create-relationship-by-path
                 (datastore tbnl:*acceptor*) (tbnl:request-uri*) dest-path)
               ;; Report success to the client
               (setf (tbnl:content-type*) "text/plain")
               (setf (tbnl:return-code*) tbnl:+http-created+)
               ;; FIXME: find a good JSON representation of what was just created
               "CREATED")
             ;; Attempted violation of db integrity
             (cl-webcat:integrity-error (e) (return-integrity-error (message e)))
             ;; Generic client errors
             (cl-webcat:client-error (e) (return-client-error (message e)))
             (neo4cl:client-error (e) (return-client-error (neo4cl:message e))))))
        ;;
        ;; Create a dependent resource
        ((and
           (equal (tbnl:request-method*) :POST)
           (> (length uri-parts) 0)
           (equal (mod (length uri-parts) 3) 1)
           (tbnl:post-parameter "uid"))
         (handler-case
           (let ((sub-uri (cl-ppcre:regex-replace
                            (getf *config-vars* :api-uri-base) (tbnl:request-uri*) ""))
                 (newtype (car (last uri-parts)))
                 (uid (tbnl:post-parameter "uid")))
             (log-message :debug (format nil "Attempting to create dependent resource ~A:~A on ~A"
                                         newtype uid sub-uri))
             (store-dependent-resource
               (datastore tbnl:*acceptor*) sub-uri (tbnl:post-parameters*))
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-created+)
             ;; FIXME: find a good JSON representation of what was just created
             "CREATED")
           ;; Attempted violation of db integrity
           (cl-webcat:integrity-error (e) (return-integrity-error (message e)))
           ;; Generic client errors
           (cl-webcat:client-error (e) (return-client-error (message e)))
           (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))))
        ;;
        ;; Re-home a dependent resource
        ((and
           (equal (tbnl:request-method*) :POST)
           (> (length uri-parts) 0)
           (equal (mod (length uri-parts) 3) 2)
           (tbnl:post-parameter "target"))
         (handler-case
           (let ((sub-uri (cl-ppcre:regex-replace
                            (getf *config-vars* :api-uri-base) (tbnl:request-uri*) "")))
             (log-message :debug (format nil "Attempting to move dependent resource ~A to ~A"
                                         sub-uri (tbnl:post-parameter "target")))
             (move-dependent-resource
               (datastore tbnl:*acceptor*)
               sub-uri
               (tbnl:post-parameter "target"))
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-created+)
             ;; FIXME: find a good JSON representation of what was just created
             "CREATED")
           ;; Attempted violation of db integrity
           (cl-webcat:integrity-error (e) (return-integrity-error (message e)))
           ;; Generic client errors
           (cl-webcat:client-error (e) (return-client-error (message e)))
           (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))))
        ;; PUT -> update something
        ;;
        ;; Resource attributes
        ((and
           (equal (tbnl:request-method*) :PUT)
           (> (length uri-parts) 0)
           (equal (mod (length uri-parts) 3) 2))
         (handler-case
           (progn
             (log-message
               :debug
               (format nil "Attempting to update attributes of resource ~{/~A~}" uri-parts))
             (update-resource-attributes
               (datastore tbnl:*acceptor*)
               uri-parts
               (tbnl:post-parameters*))
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-created+)
             ;; Return JSON representation of the newly-updated resource
             (cl-json:encode-json-alist-to-string
               (get-resources (datastore tbnl:*acceptor*) (tbnl:request-uri*))))
           ;; Attempted violation of db integrity
           (cl-webcat:integrity-error (e) (return-integrity-error (message e)))
           ;; Generic client errors
           (cl-webcat:client-error (e) (return-client-error (message e)))
           (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))))
        ;;
        ;; DELETE -> Delete something
        ;;
        ;; Resource
        ((and (equal (tbnl:request-method*) :DELETE)
              (equal (mod (length uri-parts) 3) 2))
         (handler-case
           (let ((sub-uri (cl-ppcre:regex-replace
                            (getf *config-vars* :api-uri-base) (tbnl:request-uri*) "")))
             (log-message :debug "Attempting to delete a resource on an arbitrary path")
             (delete-resource-by-path
               (datastore tbnl:*acceptor*)
               sub-uri
               :recursive (tbnl:post-parameter "recursive"))
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-no-content+)
             "")
           ;; Attempted violation of db integrity
           (cl-webcat:integrity-error (e) (return-integrity-error (message e)))
           ;; Generic client errors
           (cl-webcat:client-error (e) (return-client-error (message e)))))
        ;;
        ;; Delete a relationship on an arbitrary path
        ((and (equal (tbnl:request-method*) :DELETE)
              (tbnl:post-parameter "resource")
              (> (length uri-parts) 3)
              (equal (mod (length uri-parts) 3) 0))
         (handler-case
           (progn
             (log-message :debug "Attempting to delete a relationship on an arbitrary path")
             (delete-relationship-by-path (datastore tbnl:*acceptor*)
                                          (tbnl:request-uri*)
                                          (tbnl:post-parameter "resource"))
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-no-content+)
             "")
           ;; Attempted violation of db integrity
           (cl-webcat:integrity-error (e) (return-integrity-error (message e)))
           ;; Generic client errors
           (cl-webcat:client-error (e) (return-client-error (message e)))))
        ;;
        ;; Methods we don't support.
        ;; Take the whitelist approach
        ((not (member (tbnl:request-method*) '(:POST :GET :PUT :DELETE)))
         (method-not-allowed))
        ;; Handle all other cases
        (t
          (return-client-error "This wasn't a valid request"))))
    ;; Handle general errors
    ;;
    ;; Generic client errors
    (neo4cl:client-error (e) (return-client-error (neo4cl:message e)))
    ;; Transient error
    (neo4cl:transient-error (e) (return-transient-error e))
    ;; Database error
    (neo4cl:database-error (e) (return-database-error e))
    ;; Service errors, e.g. connection refused
    (neo4cl:service-error (e) (return-service-error (neo4cl:message e)))))


;; Appserver startup/shutdown

(defun make-default-acceptor ()
  "Return an instance of 'cl-webcat-acceptor, a subclass of tbnl:easy-acceptor."
  (make-instance 'cl-webcat-acceptor
                 :address (or (sb-ext:posix-getenv "LISTEN_ADDR")
                              (getf *config-vars* :listen-address))
                 :port (or (when (sb-ext:posix-getenv "LISTEN_PORT")
                             (parse-integer (sb-ext:posix-getenv "LISTEN_PORT")))
                           (getf *config-vars* :listen-port))
                 :url-base (getf *config-vars* ::url-base)
                 ;; Send all logs to STDOUT, and let Docker sort 'em out
                 :access-log-destination (make-synonym-stream 'cl:*standard-output*)
                 :message-log-destination (make-synonym-stream 'cl:*standard-output*)
                 ;; Restagraph connection details
                 :rg-server (make-rg-server
                              :hostname (or (sb-ext:posix-getenv "RG_HOSTNAME")
                                            (getf *config-vars* :rg-hostname))
                              :port (or (parse-integer (sb-ext:posix-getenv "RG_PORT"))
                                        (getf *config-vars* :rg-port))
                              :raw-base (or (sb-ext:posix-getenv "RG_RAW_BASE")
                                            (getf *config-vars* :api-uri-base))
                              :schema-base (or (sb-ext:posix-getenv "RG_SCHEMA_BASE")
                                               (getf *config-vars* :schema-uri-base)))))

(defun startup (&key acceptor docker)
  "Start up the appserver.
   Ensures the uniqueness constraint on resource-types is present in Neo4j.
   Keyword arguments:
   - acceptor = prebuilt acceptor, to use instead of the default.
   - dispatchers = extra dispatchers to add to tbnl:*dispatch-table* in addition to the defaults.
   - docker = whether to start up in a manner suitable to running under docker,
   i.e. return only after Hunchentoot shuts down, instead of immediately after it starts up."
  (log-message :info "Attempting to start up the cl-webcat application server")
  ;; Sanity-check: is an acceptor already running?
  ;;; We can't directly check whether this acceptor is running,
  ;;; so we're using the existence of its special variable as a proxy.
  (if (boundp '*cl-webcat-acceptor*)
      ;; There's an acceptor already in play; bail out.
      (log-message :fatal "Acceptor already exists; refusing to create a new one.")
      ;; No existing acceptor; we're good to go.
      (progn
        ;; Figure out whether we have a schema directory to work with
        ;; Ensure we have an acceptor to work with
        (unless acceptor (setf acceptor (make-default-acceptor)))
        ;; Make it available as a dynamic variable, for shutdown to work on
        (defparameter *cl-webcat-acceptor* acceptor)
        ;; Set the dispatch table
        (cl-webcat:log-message :info "Configuring the dispatch table")
        (setf tbnl:*dispatch-table*
              (list
                ;; Include the additional dispatchers here
                (tbnl:create-regex-dispatcher "/healthcheck$" 'healthcheck)
                (tbnl:create-regex-dispatcher "/$" 'root)
                ;; Default fallback
                (tbnl:create-prefix-dispatcher "/" 'four-oh-four)))
        ;; Start up the server
        (log-message :info "Starting up Hunchentoot to serve HTTP requests")
        (handler-case
          (tbnl:start acceptor)
          (usocket:address-in-use-error
            () (log-message :error
                            (format nil "Attempted to start an already-running instance!"))))
        (when docker
          (sb-thread:join-thread
            (find-if
                    (lambda (th)
                      (string= (sb-thread:thread-name th)
                               (format nil "hunchentoot-listener-~A:~A"
                                       (tbnl:acceptor-address acceptor)
                                       (tbnl:acceptor-port acceptor))))
                    (sb-thread:list-all-threads)))))))

(defun dockerstart ()
    (startup :docker t))

(defun save-image (&optional (path "/tmp/cl-webcat"))
  (sb-ext:save-lisp-and-die path :executable t :toplevel 'cl-webcat::dockerstart))

(defun shutdown ()
  ;; Check whether there's something to shut down
  (if (and
        (boundp '*cl-webcat-acceptor*)
        *cl-webcat-acceptor*)
      ;; There is; go ahead
      (progn
      ;; Check whether it's still present but shutdown
      (if (tbnl::acceptor-shutdown-p *cl-webcat-acceptor*)
          (log-message :info "Acceptor was present but already shut down.")
          (progn
            (cl-webcat:log-message
              :info
              (format nil "Shutting down the cl-webcat application server"))
            (handler-case
              ;; Perform a soft shutdown: finish serving any requests in flight
              (tbnl:stop *cl-webcat-acceptor* :soft t)
              ;; Catch the case where it's already shut down
              (tbnl::unbound-slot
                ()
                (cl-webcat:log-message
                  :info
                  "Attempting to shut down Hunchentoot, but it's not running."))
              (sb-pcl::no-applicable-method-error
                ()
                (cl-webcat:log-message
                  :info
                  "Attempted to shut down Hunchentoot, but received an error. Assuming it wasn't running.")))))
        ;; Nuke the acceptor
        (makunbound '*cl-webcat-acceptor*))
      ;; No acceptor. Note the fact and do nothing.
      (cl-webcat:log-message :warn "No acceptor present; nothing to shut down.")))
