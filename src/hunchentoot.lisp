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
              :initform (make-acceptor))
   (url-base :initarg :url-base
             :reader url-base
             :initform "localhost")
   (template-path :initarg :template-path
                  :reader template-path
                  :initform "/opt/cl-webcat/templates"))
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

(defun get-uri-parts (uri acceptor)
  "Break the URI into parts for processing by uri-node-helper.
  Expects a string; returns a list of strings."
  (mapcar
    #'sanitise-uid
    (cdr
        (ppcre:split "/"
                     (cl-ppcre:regex-replace
                       (url-base acceptor)
                       uri "")))))

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
  "Make a GET request to a Restagraph backend that should return a JSON response.
  Return the decoded JSON.
  Arguments:
  - rg-server object
  - URI
  - :schema-p = whether this is a schema query instead of a resource one"
  (log-message :debug "Requesting URI '~A' from the ~A API" uri (if schema-p "schema" "raw"))
  (decode-json-response
    (drakma:http-request (format nil "http://~A:~D~A~A"
                                 (rg-server-hostname server)
                                 (rg-server-port server)
                                 (if schema-p
                                   (rg-server-schema-base server)
                                   (rg-server-raw-base server))
                                 uri))))

(defun rg-post-json (server uri &key payload schema-p put-p)
  "Make a POST rquest to a Restagraph backend, and decode the JSON response if there was one.
   Arguments:
   - rg-server object
   - URI
   - :payload = Drakma-ready alist of values to POST
   - :schema-p = whether this is updating the schema instead of a resource"
  (log-message :debug "~Aing a request to URI ~A" (if put-p "PUT" "POST") uri)
  (log-message :debug "Payload: ~A" payload)
  (multiple-value-bind (body status-code headers)
    (drakma:http-request
      (format nil "http://~A:~D~A~A"
              (rg-server-hostname server)
              (rg-server-port server)
              (if schema-p
                  (rg-server-schema-base server)
                  (rg-server-raw-base server))
              uri)
      :parameters payload
      :method (if put-p :PUT :POST))
    ;; Now decide what to do with it.
    ;; If it was successful, return it
    (if (and (> status-code 199)
             (< status-code 300))
        (values
          (if (equal (cdr (assoc :content-type headers))
                     "application/json")
              ;; If it's JSON, decode it
              (decode-json-response body)
              ;; If not JSON, just return the boy
              body)
          status-code)
        (values body status-code))))

(defun search-for-resources (server rtype &optional params)
  "Search in the backend for a thing.
  Expected to be called from the search page"
  (log-message :debug "Searching for ~A with parameters ~A" rtype params)
  (let ((query-string (if params
                        (format nil "/~A?~{~A~^&~}" rtype params)
                        (format nil "/~A" rtype))))
    ;(log-message :debug "search-for-resources using query string '~A'" query-string)
    (let ((result (rg-request-json server query-string)))
      (if (equal result "")
        nil
        result))))

(defun get-uids (server resourcetype)
  "Retrieve a list of UIDs for a specified resourcetype.
  Arguments:
  - server = instance of rg-server struct
  - resourcetype = string
  Returns a list of strings."
  (log-message :debug "Retrieving UIDs for ~A" resourcetype)
  (mapcar #'(lambda (item)
              (cdr (assoc :uid item)))
          (rg-request-json server (format nil "/~A" resourcetype))))

(defun get-schema (server &optional (resourcetype ""))
  (if (equal resourcetype "")
    (log-message :debug "Retrieving the whole schema")
    (log-message :debug "Retrieving the schema for resourcetype '~A'" resourcetype))
  (rg-request-json server (format nil "/~A" resourcetype) :schema-p t))

(defun get-attrs (server resourcetype)
  "Retrieve a list of attributes for a resourcetype.
   Arguments:
   - server = instance of rg-server struct
   - resourcetype = string
   Returns a list of strings."
  (cdr (assoc :attributes
              (get-schema server resourcetype))))

(defun get-resourcetypes (server)
  "Retrieve a sorted list of resourcetypes.
   Filter out rgResource, rgAttribute et al."
  (sort
    (mapcar #'(lambda (rtype)
                (cdr (assoc :name rtype)))
            (get-schema server))
    #'string<))

(defun search-results-to-template (res)
  "Transform the output of a search into something digestible by html-template.
  Accepts a list of alists.
  Returns a list of plists."
  (log-message :debug "search-results-to-template formatting results ~A" res)
  (mapcar #'(lambda (result)
              `(:uid ,(cdr (assoc :uid result))))
          res))


;; Request handlers

(defun root ()
  "Default handler for requests to /"
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-ok+)
  "OK")

(defun healthcheck ()
  "Basic is-it-running endpoint"
  (log-message :debug "Returning healthcheck")
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-ok+)
  "OK")

#|
This seems really stupid, but it should work.
From inside to outside:
- replace __ with / to put it safely out of the way.
- replace _ with a space.
- replace / with _.
Now double-underscores have turned into single underscores,
single underscores have turned into spaces,
and any forward-slashes that sneaked through are also now underscores.
|#
(defun uid-to-title (uid)
  "Render the UID as a human-friendly title string"
  (cl-ppcre:regex-replace-all
    "/"
    (cl-ppcre:regex-replace-all
      "_"
      (cl-ppcre:regex-replace-all "__" uid "/")
      " ")
    "_"))

(defun display-item ()
  "Display an item"
  (log-message :debug "Handling display request from URI ~A" (tbnl:request-uri*))
  (let* ((uri-parts (get-uri-parts (tbnl:request-uri*) tbnl:*acceptor*))
         (resourcetype (second uri-parts))
         (uid (third uri-parts))
         (content (rg-request-json (rg-server tbnl:*acceptor*)
                                   (format nil "/~A/~A" resourcetype uid)))
         (schema (mapcar #'(lambda (attr) (intern (string-upcase attr) 'keyword))
                         (cdr (assoc :attributes
                                     (rg-request-json (rg-server tbnl:*acceptor*)
                                                      (format nil "/~A" resourcetype)
                                                      :schema-p t))))))
    (log-message :debug "Content: ~A" content)
    (log-message :debug "Schema ~A" schema)
    (if (and content schema)
      (let ((filtered-content
              (mapcar #'(lambda (attrname)
                          (list :attrname attrname
                                :attrval (or (cdr (assoc attrname content)) "")))
                      (sort schema #'string<)))
            (layout-template-path (concatenate 'string
                                               (template-path tbnl:*acceptor*)
                                               "/display_layout.tmpl"))
            (html-template:*string-modifier* #'cl:identity))
        (log-message :debug "Filtered content: ~A" filtered-content)
        (setf (tbnl:content-type*) "text/html")
        (setf (tbnl:return-code*) tbnl:+http-ok+)
        (with-output-to-string (outstr)
          (html-template:fill-and-print-template
            (make-pathname :defaults layout-template-path)
            (list :resourcetype resourcetype
                  :uid uid
                  ;; If it's a wikipage _and_ it has a title, use that.
                  ;; Otherwise, just de-url-escape the UID
                  :title (if (and
                               (equal resourcetype "wikipages")
                               (assoc :title filtered-content))
                           (cdr (assoc :title filtered-content))
                           (uid-to-title uid))
                  :content (if (equal resourcetype "wikipages")
                             ;; Display a wikipage
                             (with-output-to-string (contstr)
                               (html-template:fill-and-print-template
                                 (make-pathname :defaults (concatenate
                                                            'string
                                                            (template-path tbnl:*acceptor*)
                                                            "/display_wikipage.tmpl"))
                                 (list :content
                                       (with-output-to-string
                                         (mdstr)
                                         (cl-markdown:markdown (cdr (assoc :text content))
                                                               :stream mdstr)))
                                 :stream contstr))
                             ;; Default item display
                             (with-output-to-string (contstr)
                               (html-template:fill-and-print-template
                                 (make-pathname :defaults (concatenate
                                                            'string
                                                            (template-path tbnl:*acceptor*)
                                                            "/display_default.tmpl"))
                                 (list :attributes filtered-content)
                                 :stream contstr))))
            :stream outstr)))
      (progn
        (setf (tbnl:content-type*) "text/plain")
        (setf (tbnl:return-code*) tbnl:+http-not-found+)
        "No content"))))

(defun edit-resource ()
  "Handle the edit-page for an item"
  (log-message :debug "Attempting to edit an item with URI ~A" (tbnl:request-uri*))
  (cond
    ((equal (tbnl:request-method*) :GET)
     (let* ((uri-parts (get-uri-parts (tbnl:request-uri*) tbnl:*acceptor*))
            (resourcetype (second uri-parts))
            (uid (third uri-parts))
            (content (rg-request-json (rg-server tbnl:*acceptor*)
                                      (format nil "/~A/~A" resourcetype uid)))
            (schema
              (sort (cdr (assoc :attributes
                                (rg-request-json (rg-server tbnl:*acceptor*)
                                                 (format nil "/~A" resourcetype)
                                                 :schema-p t))) #'string<)))
       (log-message :debug "Resourcetype: ~A" resourcetype)
       (log-message :debug "UID: ~A" uid)
       (log-message :debug "Schema ~A" schema)
       (log-message :debug "Content: ~A" content)
       (if (and content schema)
           (if (equal resourcetype "wikipages")
               (progn
                 (log-message :debug "Rendering wikipage ~A" uid)
                 (setf (tbnl:content-type*) "text/html")
                 (setf (tbnl:return-code*) tbnl:+http-ok+)
                 (with-output-to-string (outstr)
                   (html-template:fill-and-print-template
                     (make-pathname :defaults (concatenate 'string
                                                           (template-path tbnl:*acceptor*)
                                                           "/edit_wikipage.tmpl"))
                     (list :title (if (and
                                        (assoc :title content)
                                        (not (equal (cdr (assoc :title content)) "")))
                                      (cdr (assoc :title content))
                                      (uid-to-title uid))
                           :uid uid
                           :content (cdr (assoc :text content)))
                     :stream outstr)))
               (let ((filtered-content
                       (mapcar #'(lambda (attrname)
                                   (list :attrname attrname
                                         :attrval (or (cdr (assoc
                                                             (intern (string-upcase attrname) 'keyword)
                                                             content)) "")))
                               schema)))
                 (log-message :debug "Filtered content: ~A" filtered-content)
                 (setf (tbnl:content-type*) "text/html")
                 (setf (tbnl:return-code*) tbnl:+http-ok+)
                 (with-output-to-string (outstr)
                   (html-template:fill-and-print-template
                     (make-pathname :defaults (concatenate 'string
                                                           (template-path tbnl:*acceptor*)
                                                           "/edit_resource.tmpl"))
                     (list :resourcetype resourcetype
                           :uid uid
                           :title (uid-to-title uid)
                           :attributes filtered-content)
                     :stream outstr))))
           (progn
             (setf (tbnl:content-type*) "text/plain")
             (setf (tbnl:return-code*) tbnl:+http-not-found+)
             "No content"))))
    ((equal (tbnl:request-method*) :POST)
     (let* ((uri-parts (get-uri-parts (tbnl:request-uri*) tbnl:*acceptor*))
            (resourcetype (second uri-parts))
            (uid (third uri-parts))
            ;; Extract attributes relevant to this resourcetype
            (validated-attrs
              (mapcar #'(lambda (attr)
                          (log-message :debug "Checking for parameter ~A" attr)
                          (let ((val (tbnl:post-parameter attr)))
                            (when val
                              (cons attr val))))
                      (cdr (assoc :attributes
                                  (rg-request-json (rg-server tbnl:*acceptor*)
                                                   (concatenate 'string "/" resourcetype)
                                                   :schema-p t))))))
       (log-message :debug "Validated attributes: ~A" validated-attrs)
       ;; Send the update
       (multiple-value-bind (body status-code)
         (rg-post-json (rg-server tbnl:*acceptor*)
                       (concatenate 'string "/" resourcetype "/" uid)
                       :payload validated-attrs
                       :put-p t)
         ;; Did it work?
         (if (and (> status-code 199)
                  (< status-code 300))
             ;; Happy path
             (tbnl:redirect (concatenate 'string "/display/" resourcetype "/" uid ))
             ;; Less-happy path
             (let ((html-template:*string-modifier* #'cl:identity))
               (setf (tbnl:content-type*) "text/html")
               (setf (tbnl:return-code*) tbnl:+http-bad-request+)
               (with-output-to-string (outstr)
                 (html-template:fill-and-print-template
                   (make-pathname :defaults (concatenate 'string
                                                         (template-path tbnl:*acceptor*)
                                                         "/display_layout.tmpl"))
                   `(:resourcetype ,resourcetype
                     :uid ,uid
                     :title ,(format nil "Failed to create ~A" uid)
                     :content ,(with-output-to-string (contstr)
                                 (html-template:fill-and-print-template
                                   (make-pathname :defaults (concatenate 'string
                                                                         (template-path tbnl:*acceptor*)
                                                                         "/display_default.tmpl"))
                                   `(:attributes ((:attrname "Server message"
                                                   :attrval ,body)))
                                   :stream contstr)))
                   :stream outstr)))))))
    ;; Fallback: not by this method
    (t (method-not-allowed))))

(defun searchpage ()
  "Display the search-page"
  (cond
    ((equal (tbnl:request-method*) :GET)
     (let* ((schema (mapcar #'(lambda (rtype)
                                (list :name rtype :selected nil))
                            (remove-if #'(lambda (name)
                                           (cl-ppcre:all-matches "^rg" name))
                                       (get-resourcetypes (rg-server tbnl:*acceptor*)))))
            (tags (mapcar #'(lambda (tag)
                              (list :tag tag
                                    :selected nil))
                          (get-uids (rg-server tbnl:*acceptor*) "tags")))
            (search-criteria (append ()
                                     (when (tbnl:get-parameter "uid_regex")
                                       (list (format nil "uid_regex=~A"
                                                     (tbnl:get-parameter "uid_regex"))))))
            (search-results
              (when (and
                      (tbnl:get-parameter "resourcetype")
                      (not (equal (tbnl:get-parameter "resourcetype") "")))
                (log-message :debug "Searching with criteria '~A'" search-criteria)
                (search-for-resources (rg-server tbnl:*acceptor*)
                                      (tbnl:get-parameter "resourcetype")
                                      search-criteria))))
       ;; Debug logging for what we've obtained so far
       (log-message :debug "Schema: ~A" schema)
       (log-message :debug "Tags: ~A" tags)
       (log-message :debug "Resourcetype supplied: ~A" (if (tbnl:get-parameter "resourcetype") "yes" "no"))
       (log-message :debug "Search results: ~A" search-results)
       (let ((tbnl-formatted-results (search-results-to-template search-results)))
         (when (tbnl:get-parameter "resourcetype")
           (log-message :debug "Search results: ~A" search-results)
           (log-message :debug "tbnl-formatted search results: ~A" tbnl-formatted-results))
         (setf (tbnl:content-type*) "text/html")
         (setf (tbnl:return-code*) tbnl:+http-ok+)
         (with-output-to-string (outstr)
           (html-template:fill-and-print-template
             (make-pathname :defaults
                            (concatenate 'string
                                         (template-path tbnl:*acceptor*)
                                         "/display_search.tmpl"))
             (list :schema schema
                   :tags tags
                   :resourcetype (tbnl:get-parameter "resourcetype")
                   :results tbnl-formatted-results)
             :stream outstr)))))
    ;; Fallback: not by this method
    (t (method-not-allowed))))

(defun createitem ()
  "Display the create-item page"
  (cond
    ((equal (tbnl:request-method*) :GET)
     (let ((schema (mapcar #'(lambda (rtype)
                               (list :name rtype :selected nil))
                           (get-resourcetypes (rg-server tbnl:*acceptor*)))))
       (setf (tbnl:content-type*) "text/html")
       (setf (tbnl:return-code*) tbnl:+http-ok+)
       (with-output-to-string (outstr)
         (html-template:fill-and-print-template
           #p"templates/display_createitem.tmpl"
           (list :schema schema)
           :stream outstr))))
    ;; Fallback: not by this method
    (t (method-not-allowed))))


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
  (log-message :warn "Client triggered integrity error: ~A" logmessage)
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-conflict+)
  ;; If we were handed a specific message, use that.
  ;; Otherwise, just pass on the logmessage.
  (if client-message client-message logmessage))

(defun return-database-error (message)
  "There was a database problem. Log it and report something generic to the user, not to keep them in the dark but to reduce the potential for data leakage."
  (log-message :error "Database error: ~A" message)
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-internal-server-error+)
  "An error occurred with the database. This has been logged, and will be fixed.")

(defun return-transient-error (message)
  "Transient problem, which may already have self-resolved.. Log it and report something generic to the user, not to keep them in the dark but to reduce the potential for data leakage."
  (log-message :error "Database error: ~A" message)
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-service-unavailable+)
  "A transient error occurred, and has been logged for us to work on. Please try your request again.")

(defun return-client-error (logmessage &optional message)
  "The client made a bad request. Return this information to them, that they may learn from their mistakes."
  (log-message :info "Client error: ~A" logmessage)
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-bad-request+)
  ;; If we were handed a specific message, use that.
  ;; Otherwise, just pass on the logmessage.
  (format nil "Client error: ~A"
          (or message logmessage)))

(defun return-service-error (logmessage &optional message)
  "There was a problem with connecting to the backend service."
  (log-message :crit "Service error: ~A" logmessage)
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


;; Appserver startup/shutdown

(defun make-acceptor (&key template-path)
  "Return an instance of 'cl-webcat-acceptor, a subclass of tbnl:easy-acceptor."
  (make-instance 'cl-webcat-acceptor
                 :address (or (sb-ext:posix-getenv "LISTEN_ADDR")
                              (getf *config-vars* :listen-address))
                 :port (or (sb-ext:posix-getenv "LISTEN_PORT")
                           (getf *config-vars* :listen-port))
                 :url-base (or (getf *config-vars* ::url-base) "")
                 :template-path (or template-path
                                    (sb-ext:posix-getenv "TEMPLATE_PATH")
                                    (getf *config-vars* :template-path))
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
                              :schema-base (or (sb-ext:posix-getenv "RG_SCHEMA_BASE")
                                               (getf *config-vars* :schema-uri-base)))))

(defun startup (&key acceptor static-path template-path docker)
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
      (log-message :critical "Acceptor already exists; refusing to create a new one.")
      ;; No existing acceptor; we're good to go.
      (let ((myacceptor (or acceptor
                            (make-acceptor :template-path template-path)))
            (static-filepath (or static-path
                                 (sb-ext:posix-getenv "STATIC_PATH")
                                 (getf *config-vars* :static-path))))
        ;; Make it available as a dynamic variable, for shutdown to work on
        (defparameter *cl-webcat-acceptor* myacceptor)
        ;; Stop html-template raising a warning every time it compiles a template
        (setf html-template:*warn-on-creation* nil)
        ;; Set the dispatch table
        (log-message :info "Configuring the dispatch table")
        (setf tbnl:*dispatch-table*
              (list
                ;; Include the additional dispatchers here
                (tbnl:create-regex-dispatcher "/create$" 'createitem)
                (tbnl:create-prefix-dispatcher "/search" 'searchpage)
                (tbnl:create-prefix-dispatcher "/display" 'display-item)
                (tbnl:create-prefix-dispatcher "/editresource" 'edit-resource)
                (tbnl:create-folder-dispatcher-and-handler "/static/css/"
                                                           (concatenate 'string static-filepath "/css/")
                                                           "text/css")
                (tbnl:create-folder-dispatcher-and-handler "/static/js/"
                                                           (concatenate 'string static-filepath "/js/")
                                                           "text/javascript")
                (tbnl:create-regex-dispatcher "/healthcheck$" 'healthcheck)
                (tbnl:create-regex-dispatcher "/$" 'root)
                ;; Default fallback
                (tbnl:create-prefix-dispatcher "/" 'four-oh-four)))
        ;; Start up the server
        (log-message :info "Starting up Hunchentoot to serve HTTP requests")
        (handler-case
          (tbnl:start myacceptor)
          (usocket:address-in-use-error
            () (log-message :error "Attempted to start an already-running instance!")))
        (when docker
          (sb-thread:join-thread
            (find-if
                    (lambda (th)
                      (string= (sb-thread:thread-name th)
                               (format nil "hunchentoot-listener-~A:~A"
                                       (tbnl:acceptor-address myacceptor)
                                       (tbnl:acceptor-port myacceptor))))
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
            (log-message :info "Shutting down the cl-webcat application server")
            (handler-case
              ;; Perform a soft shutdown: finish serving any requests in flight
              (tbnl:stop *cl-webcat-acceptor* :soft t)
              ;; Catch the case where it's already shut down
              (tbnl::unbound-slot
                ()
                (log-message :info "Attempting to shut down Hunchentoot, but it's not running."))
              (sb-pcl::no-applicable-method-error
                ()
                (log-message
                  :info
                  "Attempted to shut down Hunchentoot, but received an error. Assuming it wasn't running.")))))
        ;; Nuke the acceptor
        (makunbound '*cl-webcat-acceptor*))
      ;; No acceptor. Note the fact and do nothing.
      (log-message :warn "No acceptor present; nothing to shut down.")))
