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
   (neo4j-server :initarg :neo4j-server
                 :reader neo4j-server
                 :initform (error "neo4j-server parameter is required"))
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
    ;; Remove any null elements returned by the split.
    ;; We only expect it to come from the leading / but make sure anyway.
    (remove-if #'(lambda (x)
                   (or (null x)
                       (equal x "")))
               ;; Break it up on the / delimiter
               (cl-ppcre:split "/"
                               ;; Trim off GET-style arguments
                               (first (cl-ppcre:split "\\?"
                                                      ;; Trim off the base URI
                                                      (cl-ppcre:regex-replace
                                                        (url-base acceptor)
                                                        uri "")))))))

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

(defun rg-delete (server uri &key payload schema-p)
  "Delete a resource from the Restagraph backend.
   Payload format: a list of 'name=value' strings.
   Return the status code as the primary value, and the message body as an additional value."
  (let ((url (format nil "http://~A:~D~A~A?~{~A~^&~}"
                     (rg-server-hostname server)
                     (rg-server-port server)
                     (if schema-p
                         (rg-server-schema-base server)
                         (rg-server-raw-base server))
                     uri
                     payload)))
    (log-message :info "DELETEing resource with URL ~A" url)
    (multiple-value-bind (body status-code)
      (drakma:http-request url :method :DELETE)
      (values status-code body))))

(defun rg-post-json (server uri &key payload schema-p put-p)
  "Make a POST request to a Restagraph backend, and decode the JSON response if there was one.
   Arguments:
   - rg-server object
   - URI
   - :payload = Drakma-ready alist of values to POST
   - :schema-p = whether this is updating the schema instead of a resource
   Return the body as the primary value, and the status-code as a secondary value."
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
  Expected to be called from the search page.
  params = optional list of strings."
  (log-message :debug "Searching for ~A with parameters ~A" rtype params)
  (let ((query-string (if params
                        (format nil "/~A?~{~A~^&~}" rtype params)
                        (format nil "/~A" rtype))))
    (log-message :debug "search-for-resources using query string '~A'" query-string)
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
   Returns a list of alists representing attributes."
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
  Returns a list of plists, with keys:
  - :uid
  - :title = UID after escaping via uid-to-title to be more human-friendly.
  Only pulls out the UID because that's the only attribute we can expect to get.
  We're currently searching specifically by resourcetype, so we already know that and
  it's not in the returned results anyway."
  (log-message :debug "search-results-to-template formatting results ~A" res)
  (mapcar #'(lambda (result)
              `(:uid ,(cdr (assoc :uid result))
                     :title  ,(uid-to-title (cdr (assoc :uid result)))))
          (sort res #'(lambda (row1 row2)
                        (string< (cdr (assoc :uid row1))
                                 (cdr (assoc :uid row2)))))))


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

(defun get-outbound-rels (server resourcetype)
  "Return a plist of all valid outgoing relationships from this resourcetype,
   including those from the 'any' type.
   Keys:
   - relationship
   - dependent-p
   - resourcetype"
  (mapcar #'(lambda (rel)
              (list :relationship (cdr (assoc :relationship rel))
                    :dependent-p (when (equal (cdr (assoc :dependent rel))
                                              "true")
                                   t)
                    :resourcetype (first (cdr (assoc :resourcetype rel)))))
          (append
            ;; ...both from this exact type...
            (cdr (assoc :relationships (get-schema server resourcetype)))
            ;; ...and from the "any" type.
            (cdr (assoc :relationships (get-schema server "any"))))))

(defun get-linked-resources (server uri-parts)
  "Retrieve a list of all resources linked from the given one.
   - server: an rg-server object
   - uri-parts: the output of get-uri-parts.
   Return an alist:
   - relationship
   - dependent-p
   - resourcetype
   - uid"
  ;; Sanity check: is this plausibly a path to a resource?
  (if (= (mod (length uri-parts) 3) 2)
      ;; Yes: carry on.
      ;; Get a list of the resources to which this resource has links.
      ;; Do this by finding out what valid outgoing relationships it has,
      ;; then asking what's at the end of each of those relationships.
      ;;
      ;; Get a list of the valid outgoing relationships from this type
      (apply #'append
             (let* ((resourcetype (car (last uri-parts 2)))
                    (rels (get-outbound-rels server resourcetype)))
               (log-message :debug "Outbound rels for resourcetype ~A: ~A"
                            resourcetype rels)
               (remove-if #'null
                          (mapcar
                            #'(lambda (rel)
                                (log-message
                                  :debug
                                  "Getting ~A resources with relationship ~A to resource ~{/~A~}"
                                  (getf rel :resourcetype) (getf rel :relationship) uri-parts)
                                ;; Create plists of the query results for returning.
                                ;; Don't bother turning them into structs or objects, because
                                ;; html-template requires plists, so we'd only have to convert
                                ;; them back again.
                                (mapcar #'(lambda (res)
                                            (list :relationship (getf rel :relationship)
                                                  :dependent-p (getf rel :dependent-p)
                                                  :resourcetype (getf rel :resourcetype)
                                                  :uid (cdr (assoc :uid res))))
                                        ;; Make the request
                                        (rg-request-json
                                          server
                                          (format nil "~{/~A~}/~A/~A"
                                                  uri-parts
                                                  (getf rel :relationship)
                                                  (getf rel :resourcetype)))))
                            rels))))
      ;; This can't be a resource
      (progn
        (log-message :error "get-linked-resources failed: ~{/~A~} can't be a resource" uri-parts)
        ;; Return nil
        nil)))

(defun display-item ()
  "Display an item"
  (log-message :debug "Handling display request from URI ~A" (tbnl:request-uri*))
  (let* ((uri-parts (get-uri-parts (tbnl:request-uri*) tbnl:*acceptor*))
         (resourcetype (second uri-parts))
         (uid (third uri-parts))
         (content (rg-request-json (rg-server tbnl:*acceptor*)
                                   (format nil "/~A/~A" resourcetype uid)))
         (attributes (mapcar #'(lambda (attr)
                                 (intern (string-upcase (car (assoc :name attr))) 'keyword))
                             (cdr (assoc :attributes
                                         (rg-request-json (rg-server tbnl:*acceptor*)
                                                          (format nil "/~A" resourcetype)
                                                          :schema-p t))))))
    (log-message :debug "Content: ~A" content)
    (log-message :debug "Resource-type atributes: ~A" attributes)
    (if content
        (let ((filtered-content
                (mapcar #'(lambda (attrname)
                            (let ((val (cdr (assoc attrname content))))
                              (list :attrname attrname
                                    ; Ensure all values are strings, for the template.
                                    :attrval (or (when val
                                                   ;; Render all descriptions as Markdown
                                                   (if (equal attrname :description)
                                                       (with-output-to-string (mdstr)
                                                         (3bmd:parse-string-and-print-to-stream val mdstr))
                                                       val))
                                                 ""))))
                        (sort attributes #'string<)))
              (layout-template-path (concatenate 'string
                                                 (template-path tbnl:*acceptor*)
                                                 "/display_layout.tmpl"))
              (html-template:*string-modifier* #'cl:identity)
              (outbound-links (get-linked-resources (rg-server tbnl:*acceptor*)
                                                    (cdr uri-parts))))
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
                    :content (cond
                               ;; Display a task
                               ((equal resourcetype "tasks")
                                (with-output-to-string (contstr)
                                  (html-template:fill-and-print-template
                                    (make-pathname :defaults (concatenate 'string
                                                                          (template-path tbnl:*acceptor*)
                                                                          "/display_task.tmpl"))
                                    (list :description (or (cdr (assoc :description content)) "(No description found)")
                                          :importance (or (cdr (assoc :importance content)) "(No importance found)")
                                          :urgency (or (cdr (assoc :urgency content)) "(No urgency found)")
                                          :scale (or (cdr (assoc :scale content)) "(No scale found)")
                                          :status (or
                                                    (cdr (assoc :uid
                                                                (car
                                                                  (rg-request-json
                                                                    (rg-server tbnl:*acceptor*)
                                                                    (format nil "/tasks/~A/Status/task_status" uid)
                                                                    :schema-p nil))))
                                                    "(No status found)"))
                                    :stream contstr)))
                               ;; Display a wikipage
                               ((equal resourcetype "wikipages")
                                (with-output-to-string (contstr)
                                  (html-template:fill-and-print-template
                                    (make-pathname :defaults (concatenate 'string
                                                                          (template-path tbnl:*acceptor*)
                                                                          "/display_wikipage.tmpl"))
                                    (list :content
                                          (with-output-to-string (mdstr)
                                            (3bmd:parse-string-and-print-to-stream
                                             (cdr (assoc :text content))
                                             mdstr)))
                                    :stream contstr)))
                               ;; Default item display
                               (t (with-output-to-string (contstr)
                                    (html-template:fill-and-print-template
                                      (make-pathname :defaults (concatenate 'string
                                                                            (template-path tbnl:*acceptor*)
                                                                            "/display_default.tmpl"))
                                      (list :attributes filtered-content)
                                      :stream contstr))))
                    :tags (remove-if-not #'(lambda (link)
                                             (equal (getf link :relationship) "Tags"))
                                         outbound-links)
                    :groups (remove-if-not #'(lambda (link)
                                               (and
                                                 (equal (getf link :resourcetype) "groups")
                                                 (equal (getf link :relationship) "Member")))
                                           outbound-links)
                    :outbound (remove-if #'(lambda (link)
                                             (or
                                               ;; Tags
                                               (equal (getf link :relationship) "Tags")
                                               ;; groups
                                               (and
                                                 (equal (getf link :resourcetype) "groups")
                                                 (equal (getf link :relationship) "Member"))))
                                         outbound-links))
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
                                                 :schema-p t)))
                    #'string<)))
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
                                                             content)) "")
                                         :textarea (when (equal attrname "description") t)))
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
                   (make-pathname :defaults
                                  (concatenate 'string
                                               (template-path tbnl:*acceptor*)
                                               "/display_layout.tmpl"))
                   `(:resourcetype ,resourcetype
                     :uid ,uid
                     :title ,(format nil "Failed to create ~A" uid)
                     :content ,(with-output-to-string (contstr)
                                 (html-template:fill-and-print-template
                                   (make-pathname
                                     :defaults
                                     (concatenate 'string
                                                  (template-path tbnl:*acceptor*)
                                                  "/display_default.tmpl"))
                                   `(:attributes ((:attrname "Server message"
                                                   :attrval ,body)))
                                   :stream contstr)))
                   :stream outstr)))))))
    ;; Fallback: not by this method
    (t (method-not-allowed))))

(defun make-simple-alist (lst tag)
  "Turn a list of atoms into a list of '(,tag <atom>) lists"
  (mapcar #'(lambda (atm)
              (list tag atm))
          lst))

(defun edit-links ()
  "Edit a resource's links to tags, groups and other resources."
  (cond
    ((equal (tbnl:request-method*) :GET)
     (let* ((uri-parts (get-uri-parts (tbnl:request-uri*) tbnl:*acceptor*))
            (resource (concatenate 'string "/" (second uri-parts) "/" (third uri-parts)))
            (extant-tags
              (mapcar #'(lambda (tag)
                          (cdr (assoc :uid tag)))
                      (rg-request-json
                        (rg-server tbnl:*acceptor*)
                        (concatenate 'string resource "/Tags/tags"))))
            (extant-groups
              (mapcar #'(lambda (group)
                          (cdr (assoc :uid group)))
                      (rg-request-json
                        (rg-server tbnl:*acceptor*)
                        (concatenate 'string resource "/Member/groups"))))
            (all-tags (get-uids (rg-server tbnl:*acceptor*) "tags"))
            (all-groups (get-uids (rg-server tbnl:*acceptor*) "groups")))
       (with-output-to-string (outstr)
         (html-template:fill-and-print-template
           (make-pathname :defaults (concatenate 'string
                                                 (template-path tbnl:*acceptor*)
                                                 "/edit_links.tmpl"))
           (list :resource resource
                 :add-tags (make-simple-alist
                             (sort
                               (set-difference all-tags extant-tags :test #'equal)
                               #'string<)
                             :tag)
                 :remove-tags (make-simple-alist (sort extant-tags #'string<) :tag)
                 :add-groups (make-simple-alist
                               (sort
                                 (set-difference all-groups extant-groups :test #'equal)
                                 #'string<)
                               :group)
                 :remove-groups (make-simple-alist (sort extant-groups #'string<) :group))
           :stream outstr))))
    ((equal (tbnl:request-method*) :POST)
     (let* ((uri-parts (get-uri-parts (tbnl:request-uri*) tbnl:*acceptor*))
            (resourcetype (second uri-parts))
            (uid (third uri-parts))
            ;; Error counter for the updates
            (update-errors ()))
       ;; Try to perform the updates
       (mapcar #'(lambda (param)
                   (cond
                     ;; Add a tag
                     ((equal (car param) "add-tags")
                      (log-message :debug "Adding tag ~A" (cdr param))
                      (multiple-value-bind (body status-code)
                        (rg-post-json (rg-server tbnl:*acceptor*)
                                      (concatenate 'string
                                                   "/" resourcetype "/" uid "/Tags")
                                      :payload `(("target"
                                                  . ,(concatenate 'string "/tags/" (cdr param)))))
                        ;; Did it work?
                        (if (or (< status-code 200)
                                (> status-code 299))
                          (push (list :attrname (concatenate 'string
                                                             "Failed to add tag " (cdr param))
                                      :attrval (format nil "~A: ~A" status-code body))
                                update-errors))))
                     ;; Add it to a group
                     ((equal (car param) "add-groups")
                      (log-message :debug "Adding to group ~A" (cdr param))
                      (multiple-value-bind (body status-code)
                        (rg-post-json (rg-server tbnl:*acceptor*)
                                      (concatenate 'string
                                                   "/" resourcetype "/" uid "/Member/")
                                      :payload `(("target"
                                                  . ,(concatenate
                                                       'string
                                                       "/groups/" (cdr param)))))
                        ;; Did it work?
                        (if (or (< status-code 200)
                                (> status-code 299))
                          (push (list :attrname (concatenate 'string
                                                             "Failed to add group " (cdr param))
                                      :attrval (format nil "~A: ~A" status-code body))
                                update-errors))))
                     ;; Remove a tag
                     ((equal (car param) "remove-tags")
                      (log-message :debug "Removing tag ~A" (cdr param))
                      (multiple-value-bind (status-code body)
                        (rg-delete (rg-server tbnl:*acceptor*)
                                   (concatenate 'string
                                                "/" resourcetype "/" uid "/Tags")
                                   :payload (list (concatenate 'string "resource=/tags/" (cdr param))))
                        ;; Did it work?
                        (if (or (< status-code 200)
                                (> status-code 299))
                          (push (list :attrname (concatenate 'string
                                                             "Failed to remove tag " (cdr param))
                                      :attrval (format nil "~A: ~A" status-code body))
                                update-errors))))
                     ;; Remove it from a group
                     ((equal (car param) "remove-groups")
                      (log-message :debug "Removing from group ~A" (cdr param))
                      (multiple-value-bind (status-code body)
                        (rg-delete (rg-server tbnl:*acceptor*)
                                   (concatenate 'string
                                                "/" resourcetype "/" uid "/Member")
                                   :payload (list (concatenate 'string "resource=/groups/" (cdr param))))
                        ;; Did it work?
                        (if (or (< status-code 200)
                                (> status-code 299))
                          (push (list :attrname (concatenate 'string
                                                             "Failed to remove group " (cdr param))
                                      :attrval (format nil "~A: ~A" status-code body))
                                update-errors))))
                     ;; Links to other resources
                     ((equal (car param) "resource_links")
                      (log-message :debug "Linking to resources '~A'" (cdr param))
                      (mapcar #'(lambda (target)
                                  (let* ((target-parts
                                           (remove-if #'(lambda (x)
                                                          (or (null x)
                                                              (equal x "")))
                                                      (cl-ppcre:split "/" target)))
                                         (sourcepath (concatenate
                                                       'string
                                                       "/" resourcetype "/" uid "/" (first target-parts)))
                                         (targetpath (format nil "~{/~A~}" (cdr target-parts))))
                                    (log-message :debug
                                                 (format nil "Linking ~A to ~A"
                                                         sourcepath targetpath))
                                    (multiple-value-bind (body status-code)
                                      (rg-post-json
                                        (rg-server tbnl:*acceptor*)
                                        sourcepath
                                        :payload `(("target" . ,targetpath)))
                                      ;; Did it work?
                                      (if (or (< status-code 200)
                                              (> status-code 299))
                                        (push (list :attrname (concatenate 'string
                                                                           "Failed to add tag " (cdr param))
                                                    :attrval (format nil "~A: ~A" status-code body))
                                              update-errors)))))
                              ;; Split on any quantity of whitespace
                              (cl-ppcre:split "[ ]+" (cdr param))))
                     ;; Something else
                     (t (log-message :debug "Other parameter supplied to edit-tags: ~A" param))))
               (tbnl:post-parameters*))
       ;; At least one of those updates broke:
       (if update-errors
         (let ((html-template:*string-modifier* #'cl:identity))
           (setf (tbnl:content-type*) "text/html")
           (setf (tbnl:return-code*) tbnl:+http-bad-request+)
           (with-output-to-string (outstr)
             (html-template:fill-and-print-template
               (make-pathname :defaults
                              (concatenate 'string
                                           (template-path tbnl:*acceptor*)
                                           "/display_layout.tmpl"))
               `(:resourcetype ,resourcetype
                               :uid ,uid
                               :title ,(format nil "Failed to create ~A" uid)
                               :content ,(with-output-to-string (contstr)
                                           (html-template:fill-and-print-template
                                             (make-pathname
                                               :defaults
                                               (concatenate 'string
                                                            (template-path tbnl:*acceptor*)
                                                            "/display_default.tmpl"))
                                             (list :attributes update-errors)
                                             :stream contstr)))
               :stream outstr)))
         ;; Happy path: no errors
         (tbnl:redirect (concatenate 'string "/display/" resourcetype "/" uid )))))
    (t (method-not-allowed))))

(defun searchpage ()
  "Display the search-page"
  (cond
    ((equal (tbnl:request-method*) :GET)
     (let* ((schema (mapcar #'(lambda (rtype)
                                (list :name rtype
                                      :selected (when (equal rtype
                                                             (tbnl:get-parameter "resourcetype"))
                                                  "selected")))
                            (remove-if #'(lambda (name)
                                           (cl-ppcre:all-matches "^rg" name))
                                       (get-resourcetypes (rg-server tbnl:*acceptor*)))))
            (tags-available (sort (get-uids (rg-server tbnl:*acceptor*) "tags") #'string<))
            (tags-requested (remove-if #'null
                                       (mapcar #'(lambda (par)
                                                   (when (equal (car par) "tags") (cdr par)))
                                               (tbnl:get-parameters*))))
            (tbnl-formatted-results
              (if (tbnl:get-parameter "resourcetype")
                (search-results-to-template
                  (let* ((requested-attributes
                           (remove-if #'null
                                      (mapcar #'(lambda (attr)
                                                  (let ((val (tbnl:get-parameter attr)))
                                                    (when val (format nil "~A=~A"
                                                                      attr val))))
                                              (mapcar #'(lambda (attralist)
                                                          (cdr (assoc :name attralist)))
                                                      (get-attrs (rg-server tbnl:*acceptor*)
                                                                 (tbnl:get-parameter "resourcetype"))))))
                         (tags-requested-formatted
                           (mapcar #'(lambda (par)
                                       (concatenate 'string
                                                    "outbound=/Tags/tags/" par))
                                   tags-requested))
                         (search-criteria (append ()
                                                  (when (tbnl:get-parameter "uid_regex")
                                                    (list (format nil "uid=~A"
                                                                  (tbnl:get-parameter "uid_regex")))))))
                    (progn
                      (log-message :debug "Searching with criteria '~A'" search-criteria)
                      (search-for-resources (rg-server tbnl:*acceptor*)
                                            (tbnl:get-parameter "resourcetype")
                                            (append tags-requested-formatted search-criteria requested-attributes)))))
                ;; If no resourcetype was specified, tbnl-formatted-results is NIL:
                ())))
       ;; Debug logging for what we've obtained so far
       (log-message :debug "Schema: ~A" schema)
       (log-message :debug "Tags: ~A" tags-available)
       (log-message :debug "Resourcetype supplied: ~A"
                    (if
                      (tbnl:get-parameter "resourcetype")
                      (tbnl:get-parameter "resourcetype") "none"))
       (log-message :debug "tbnl-formatted search results: ~A" tbnl-formatted-results)
       (setf (tbnl:content-type*) "text/html")
       (setf (tbnl:return-code*) tbnl:+http-ok+)
       (with-output-to-string (outstr)
         (html-template:fill-and-print-template
           (make-pathname :defaults
                          (concatenate 'string
                                       (template-path tbnl:*acceptor*)
                                       "/display_search.tmpl"))
           (list :schema schema
                 :tags (mapcar #'(lambda (tag)
                                   (list :tag tag
                                         :selected (when (member tag tags-requested :test #'equal)
                                                     "selected")))
                               tags-available)
                 :resourcetype (tbnl:get-parameter "resourcetype")
                 :uid-regex (or (tbnl:get-parameter "uid_regex") "")
                 :results tbnl-formatted-results)
           :stream outstr))))
    ;; Fallback: not by this method
    (t (method-not-allowed))))

(defun get-task-tags (db)
  "Return a list of tags applied to existing tasks"
  (declare (type neo4cl:neo4j-rest-server db))
  (mapcar #'car
          (neo4cl:extract-rows-from-get-request
            (neo4cl:neo4j-transaction
              db
              '((:STATEMENTS
                  ((:STATEMENT . "MATCH (t:tasks)-[:Tags]->(n:tags) RETURN DISTINCT n.uid ORDER BY n.uid"))))))))

(defun search-for-tasks (db tags statuses)
  "Search for tasks using the requested parameters"
  (declare (type neo4cl:neo4j-rest-server db)
           (type list tags)         ; List of strings
           (type list statuses))    ; List of strings
  (log-message :debug "Searching for tasks with tags ~{~A~^, ~} and statuses ~{~A~^, ~}" tags statuses)
  (let* ((tag-clause (if tags (format nil "t.uid IN [~{\"~A\"~^, ~}]" tags) ""))
         (status-clause (if statuses (format nil "n.status IN [~{\"~A\"~^, ~}]" statuses) ""))
         (where-clause (if (or tags statuses)
                           (format nil "WHERE ~A~A~A"
                                   tag-clause
                                   (if (and tags statuses) " AND " "")
                                   status-clause)
                           ""))
         (query (format nil "MATCH (n:tasks)-[:Tags]->(t:tags) ~A RETURN DISTINCT n.uid, n.description, n.scale, n.importance, n.urgency ORDER BY n.uid"
                        where-clause)))
    (log-message :debug "Using query string '~A'" query)
    (mapcar #'(lambda (row)
                `(:uid ,(first row) :title ,(uid-to-title (first row))))
            (neo4cl:extract-rows-from-get-request
              (neo4cl:neo4j-transaction
                db
                `((:STATEMENTS
                    ((:STATEMENT . ,query)))))))))

(defun get-enum-vals (attr attrlist)
  "Extract the enum values for an attribute, from the list of alists returned by get-attrs"
  (log-message :debug (format nil "Extracting values for '~A' from attribute-list ~A" attr attrlist))
  (when (car attrlist)
    (if (equal attr (cdr (assoc :NAME (car attrlist))))
        (when (stringp (cdr (assoc :VALS (car attrlist))))
          (cl-ppcre:split "," (cdr (assoc :VALS (car attrlist)))))
        ;; If not this one, try the next
        (get-enum-vals attr (cdr attrlist)))))

(defun tasks ()
  "Display the tasks page"
  (cond
    ((equal (tbnl:request-method*) :GET)
     (let* ((task-attrs (get-attrs (rg-server tbnl:*acceptor*) "tasks"))
            (statuses-requested (remove-if #'null
                                           (mapcar #'(lambda (par)
                                                       (when (equal (car par) "status") (cdr par)))
                                                   (tbnl:get-parameters*))))
            (tags-available (get-task-tags (cl-webcat::neo4j-server cl-webcat::*cl-webcat-acceptor*)))
            (tags-requested (remove-if #'null
                                       (mapcar #'(lambda (par)
                                                   (when (equal (car par) "tags") (cdr par)))
                                               (tbnl:get-parameters*))))
            (tbnl-formatted-results
              (search-for-tasks (cl-webcat::neo4j-server cl-webcat::*cl-webcat-acceptor*)
                                tags-requested
                                statuses-requested)))
       ;; Debug logging for what we've obtained so far
       (log-message :debug "Attributes: ~A" task-attrs)
       (log-message :debug "Statuses available: ~A" (get-enum-vals "status" task-attrs))
       (log-message :debug "Tags: ~A" tags-available)
       (log-message :debug "tbnl-formatted search results: ~A" tbnl-formatted-results)
       (setf (tbnl:content-type*) "text/html")
       (setf (tbnl:return-code*) tbnl:+http-ok+)
       (with-output-to-string (outstr)
         (html-template:fill-and-print-template
           (make-pathname :defaults
                          (concatenate 'string
                                       (template-path tbnl:*acceptor*)
                                       "/display_tasks_search.tmpl"))
           (list :statuses (mapcar
                             #'(lambda (status)
                                 (list :name status
                                       :selected (when (member status statuses-requested :test #'equal)
                                                   "selected")))
                             (get-enum-vals "status" task-attrs))
                 :importance (mapcar
                               #'(lambda (imp)
                                   (list :name imp
                                         :selected (when (equal imp (tbnl:get-parameter "importance")) "selected")))
                               (get-enum-vals "importance" task-attrs))
                 :urgency (mapcar
                               #'(lambda (urge)
                                   (list :name urge
                                         :selected (when (equal urge (tbnl:get-parameter "urgency")) "selected")))
                               (get-enum-vals "urgency" task-attrs))
                 :scale (mapcar
                               #'(lambda (scale)
                                   (list :name scale
                                         :selected (when (equal scale (tbnl:get-parameter "scale")) "selected")))
                               (get-enum-vals "scale" task-attrs))
                 :tags (mapcar #'(lambda (tag)
                                   (list :tag tag
                                         :selected (when (member tag tags-requested :test #'equal)
                                                     "selected")))
                               tags-available)
                 :resourcetype (tbnl:get-parameter "resourcetype")
                 :uid-regex (or (tbnl:get-parameter "uid_regex") "")
                 :results tbnl-formatted-results)
           :stream outstr))))
    ;; Fallback: not by this method
    (t (method-not-allowed))))

(defun create-item ()
  "Display the create-item page"
  (cond
    ((equal (tbnl:request-method*) :GET)
     (log-message :debug "Handling create request ~A" (tbnl:request-uri*))
     (let ((schema (mapcar #'(lambda (rtype)
                               (list :name rtype :selected nil))
                           (get-resourcetypes (rg-server tbnl:*acceptor*)))))
       (log-message :debug "Retrieved schema data.")
       (setf (tbnl:content-type*) "text/html")
       (setf (tbnl:return-code*) tbnl:+http-ok+)
       (with-output-to-string (outstr)
         (html-template:fill-and-print-template
           #p"templates/display_createitem.tmpl"
           (list :schema schema)
           :stream outstr))))
    ((equal (tbnl:request-method*) :POST)
     (let ((uid (tbnl:post-parameter "uid"))
           (resourcetype (tbnl:post-parameter "resourcetype")))
       ;; Check for the UID and resourcetype; if we don't have those, give up now
       ;; Missing both of them
       (cond ((and (or (not uid)
                       (equal uid ""))
                   (or (not uid)
                       (equal uid "")))
              (setf (tbnl:content-type*) "text/plain")
              (setf (tbnl:return-code*) tbnl:+http-bad-request+)
              "Both the UID and the resourcetype must be specified")
             ;; Missing only the UID
             ((or (not uid)
                  (equal uid ""))
              (setf (tbnl:content-type*) "text/plain")
              (setf (tbnl:return-code*) tbnl:+http-bad-request+)
              "The UID parameter must be supplied")
             ;; Missing only the resourcetype
             ((or (not resourcetype)
                  (equal resourcetype ""))
              (setf (tbnl:content-type*) "text/plain")
              (setf (tbnl:return-code*) tbnl:+http-bad-request+)
              "The resourcetype must be specified in the URL")
             ;; We have both; carry on
             (t
               ;; Extract attributes relevant to this resourcetype
               (let ((validated-attrs
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
                                 (concatenate 'string "/" resourcetype)
                                 :payload (append `(("uid" . ,uid)) validated-attrs))
                   ;; Did it work?
                   (if (and (> status-code 199)
                            (< status-code 300))
                     ;; Happy path
                     (tbnl:redirect (concatenate 'string "/display/" resourcetype "/" (sanitise-uid uid)))
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
                                                         (make-pathname
                                                           :defaults
                                                           (concatenate 'string
                                                                        (template-path tbnl:*acceptor*)
                                                                        "/display_default.tmpl"))
                                                         `(:attributes ((:attrname "Server message"
                                                                                   :attrval ,body)))
                                                         :stream contstr)))
                           :stream outstr))))))))))
    ;; Fallback: not by this method
    (t (method-not-allowed))))


;; Error response functions

(defun four-oh-four ()
  "Fallthrough handler, for anything we haven't already defined."
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-not-found+)
  "cl-webcat: this is not a valid URI")

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
                                               (getf *config-vars* :schema-uri-base)))
                 :neo4j-server (make-instance
                                 'neo4cl:neo4j-rest-server
                                 :hostname (or (sb-ext:posix-getenv "NEO4J_HOSTNAME")
                                               (getf *config-vars* :dbhostname))
                                 :port (or (sb-ext:posix-getenv "NEO4J_PORT")
                                           (getf *config-vars* :dbport))
                                 :dbname (or (when (sb-ext:posix-getenv "RG_DBNAME")
                                               (sb-ext:posix-getenv "RG_DBNAME"))
                                             (getf *config-vars* :dbname))
                                 :dbuser (or (sb-ext:posix-getenv "NEO4J_USER")
                                             (getf *config-vars* :dbusername))
                                 :dbpasswd (or (sb-ext:posix-getenv "NEO4J_PASSWORD")
                                               (getf *config-vars* :dbpasswd)))))

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
                (tbnl:create-regex-dispatcher "/create$" 'create-item)
                (tbnl:create-prefix-dispatcher "/search" 'searchpage)
                (tbnl:create-prefix-dispatcher "/tasks" 'tasks)
                (tbnl:create-prefix-dispatcher "/display" 'display-item)
                (tbnl:create-prefix-dispatcher "/editresource" 'edit-resource)
                (tbnl:create-prefix-dispatcher "/edit_links" 'edit-links)
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
