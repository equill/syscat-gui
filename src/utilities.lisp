;   Copyright 2020 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

(in-package #:webcat-gui)


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

;;; Data retrieval functions

(defun decode-json-response (json)
  "Parse the JSON returned as application/json into a CL structure"
  ;; Neo4j sends a stream of octets. Convert this into a string.
  (let ((json-string (flexi-streams:octets-to-string json :external-format :UTF-8)))
    (log-message :debug (format nil "decode-json-response received string '~A'" json-string))
    ;; If an empty string was returned, pass an empty string back.
    (if (equal json-string "")
      ""
      ;; If we received actual content, on the other hand, decode it.
      (cl-json:decode-json-from-string json-string))))

(defun rg-request-json (server uri &key (api "raw"))
  "Make a GET request to a Restagraph backend that should return a JSON response.
  Return the decoded JSON.
  Arguments:
  - rg-server object
  - URI
  - :api = which API to invoke: schema, files or raw (default)."
  (log-message :debug (format nil "Requesting URI '~A' from the ~A API" uri api))
  (let ((url (format nil "http://~A:~D~A~A"
                     (rg-server-hostname server)
                     (rg-server-port server)
                     (cond
                       ((equal "schema" api)
                        (rg-server-schema-base server))
                       ((equal "files" api)
                        (rg-server-files-base server))
                       (t   ; default is "raw"
                         (rg-server-raw-base server)))
                     ;; Auto-prepend a leading slash to the URI for a schema request
                     (if (equal "schema" api)
                       (format nil "/~A" uri)
                       uri))))
    (log-message :debug (format nil "Using URL '~A'" url))
    (multiple-value-bind (body status-code)
      (drakma:http-request url :external-format-in :UTF-8)
      (if (equal 200 status-code)
        (decode-json-response body)
        (error "Non-200 status code received")))))

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
    (log-message :info (format nil "DELETEing resource with URL ~A" url))
    (multiple-value-bind (body status-code)
      (drakma:http-request url :method :DELETE :external-format-in :UTF-8)
      (values status-code body))))

(defun post-encode-payload (payload)
  "Transform a list of dotted pairs of strings into a POST-encoded string.
   The intent is to provide Drakma with pre-encoded content,
   instead of having it URL-encode the content of a PUT request.
   This also bypasses the 1024-character limit that comes with URL-encoding."
  (format nil "~{~A~^&~}"
          (mapcar #'(lambda (pair)
                      (format nil "~A=~A"
                              (drakma:url-encode (car pair) :UTF-8)
                              (drakma:url-encode (cdr pair) :UTF-8)))
                  payload)))

(defun rg-upload-file (server payload)
  "Upload a file to a Restagraph backend."
  (log-message :debug (format nil "Uploading a file"))
  (multiple-value-bind (body status-code headers)
    (drakma:http-request
      (format nil "http://~A:~D~A~A"
              (rg-server-hostname server)
              (rg-server-port server)
              (rg-server-files-base server)
              "/files/v1")
      :parameters payload
      :form-data t
      :method :POST
      :external-format-in :UTF-8
      :external-format-out :UTF-8)
    ;; Now decide what to do with it.
    ;; If it was successful, return it
    (log-message :debug (format nil "Response status-code: ~A" status-code))
    (log-message :debug (format nil "Response headers: ~A" headers))
    (if (and (> status-code 199)
             (< status-code 300))
        ;; Success!
        (progn
          (log-message :debug "Request succeeded.")
          (values
            (if (equal (cdr (assoc :content-type headers))
                       "application/json")
                ;; If it's JSON, decode it
                (decode-json-response body)
                ;; If not JSON, just return the body
                body)
            status-code))
        ;; Failure!
        (progn
          (log-message :debug (format nil "Failure: ~A ~A" status-code body))
          (values body status-code)))))

(defun rg-post-json (server uri &key payload put-p api)
  "Make a POST request to a Restagraph backend, and decode the JSON response if there was one.
  Arguments:
  - rg-server object
  - URI
  - :payload = Drakma-ready alist of values to POST
  - :put-p = whether we're invoking the PUT method
  - :api = whether to invoke the schema, files or raw (default) API.
  Return the body as the primary value, and the status-code as a secondary value."
  (log-message :debug (format nil "rg-post-json received payload: ~A" payload))
  (let ((encoded-payload (post-encode-payload payload))
        (method (if put-p :PUT :POST)))
    (log-message :debug (format nil "~Aing a request to URI ~A" method uri))
    (log-message :debug (format nil "Encoded payload: ~A" encoded-payload))
    (multiple-value-bind (body status-code headers)
      (drakma:http-request
        (format nil "http://~A:~D~A~A"
                (rg-server-hostname server)
                (rg-server-port server)
                (cond
                  ((equal "schema" api)
                   (rg-server-schema-base server))
                  ((equal "files" api)
                   (rg-server-files-base server))
                  (t   ; default is "raw"
                    (rg-server-raw-base server)))
                uri)
        ;:parameters payload
        :content encoded-payload
        :form-data (equal "files" api)
        :method method
        :external-format-in :UTF-8
        :external-format-out :UTF-8)
      ;; Now decide what to do with it.
      ;; If it was successful, return it
      (log-message :debug (format nil "Response status-code: ~A" status-code))
      (log-message :debug (format nil "Response headers: ~A" headers))
      (if (and (> status-code 199)
               (< status-code 300))
        ;; Success!
        (progn
          (log-message :debug "Request succeeded.")
          (values
            (if (equal (cdr (assoc :content-type headers))
                       "application/json")
              ;; If it's JSON, decode it
              (decode-json-response body)
              ;; If not JSON, just return the body
              body)
            status-code))
        ;; Failure!
        (progn
          (log-message :debug (format nil "Failure: ~A ~A" status-code body))
          (values body status-code))))))

(defun search-for-resources (server rtype &optional params)
  "Search in the backend for a thing.
  Expected to be called from the search page.
  params = optional list of strings."
  (log-message :debug (format nil "Searching for ~A with parameters ~A" rtype params))
  (let ((query-string (if params
                        (format nil "/~A?~{~A~^&~}" rtype params)
                        (format nil "/~A" rtype))))
    (log-message :debug (format nil "search-for-resources using query string '~A'" query-string))
    (let ((result (rg-request-json server query-string)))
      (if (equal result "")
        nil
        result))))

(defun get-uids (server query)
  "Retrieve a list of UIDs for a specified resourcetype.
  Arguments:
  - server = instance of rg-server struct
  - resourcetype = string
  Returns a list of strings."
  (log-message :debug (format nil "Retrieving UIDs for query '~A'" query))
  (mapcar #'(lambda (item)
              (cdr (assoc :uid item)))
          (rg-request-json server query)))

(defun get-schema (server &optional (resourcetype ""))
  (if (equal resourcetype "")
    (log-message :debug "Retrieving the whole schema")
    (log-message :debug (format nil "Retrieving the schema for resourcetype '~A'" resourcetype)))
  (rg-request-json server resourcetype :api "schema"))


(defun get-attrs (server resourcetype)
  "Retrieve a list of attributes for a resourcetype.
  Arguments:
  - server = instance of rg-server struct
  - resourcetype = string
  Returns a list of schema-rtype-attrs structs."
  (declare (type rg-server)
           (type string resourcetype))
  (log-message :debug (format nil "Fetching attributes for resourcetype '~A'" resourcetype))
  (let* ((rawdata (get-schema server resourcetype))
         (attributes (sort (cdr (assoc :attributes rawdata))
                           #'string<
                           :key #'(lambda (attr) (cdr (assoc :name attr))))))
    (log-message :debug (format nil "Retrieved attributes: ~A" attributes))
    (mapcar #'(lambda (attribute)
                (make-schema-rtype-attrs
                  :name (cdr (assoc :name attribute))
                  :description (or (cdr (assoc :description attribute)) "")
                  :values (cdr (assoc :values attribute))))
            attributes)))

(defun get-attrs-with-keywords (server resourcetype)
  "Return a sorted alist of the attribute definitions for a resourcetype,
  for passing to html-template.
  Key = attribute name, interned into the keyword package.
  Value = schema-rtype-attrs struct."
  (mapcar #'(lambda (attribute)
              (cons (intern (string-upcase (schema-rtype-attrs-name attribute)) 'keyword)
                    attribute))
          (get-attrs server resourcetype)))

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
  (log-message :debug (format nil "search-results-to-template formatting results ~A" res))
  (mapcar #'(lambda (result)
              `(:uid ,(cdr (assoc :uid result))
                     :title  ,(uid-to-title (cdr (assoc :uid result)))))
          (sort res #'(lambda (row1 row2)
                        (string< (cdr (assoc :uid row1))
                                 (cdr (assoc :uid row2)))))))

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
              (make-instance 'outbound-rels
                             :name (cdr (assoc :NAME rel))
                             :dependent-p (when (equal (cdr (assoc :DEPENDENT rel))
                                                       "true")
                                            t)
                             :target-type (cdr (assoc :TARGET-TYPE rel))))
          (append
            ;; ...both from this exact type...
            (cdr (assoc :relationships (get-schema server resourcetype)))
            ;; ...and from the "any" type.
            (cdr (assoc :relationships (get-schema server "any"))))))

(defun get-linked-resources (server uri-parts)
  "Retrieve a list of all resources linked from the given one.
  - server: an rg-server object
  - uri-parts: the output of get-uri-parts.
  Return a list of `linked-resource` instances."
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
             (log-message :debug (format nil "Outbound rels for resourcetype ~A: ~{~A~^, ~}"
                                         resourcetype
                                         (mapcar #'(lambda (rel)
                                                     (format nil "~A/~A (dependent: ~A)"
                                                             (name rel)
                                                             (target-type rel)
                                                             (if (dependent-p rel) "yes" "no")))
                                                 rels)))
             (mapcar
               #'(lambda (rel)
                   (log-message
                     :debug
                     (format nil "Getting ~A resources with relationship ~A from resource ~{/~A~}"
                             (target-type rel) (name rel) uri-parts))
                   (mapcar #'(lambda (res)
                               (make-instance 'linked-resource :relationship (name rel)
                                              :dependent-p (dependent-p rel)
                                              :target-type (if (equal "any" (target-type rel))
                                                             (cdr (assoc :TYPE res))
                                                             (target-type rel))
                                              :uid (cdr (assoc :uid res))))
                           ;; Make the request
                           (handler-case
                             (rg-request-json
                               server
                               (if (equal "any" (target-type rel))
                                 ;; FIXME: this isn't right. We need to handle these as returning a list,
                                 ;; complete with :resourcetype as a field in each object.
                                 (format nil "~{/~A~}/~A" uri-parts (name rel))
                                 (format nil "~{/~A~}/~A/~A" uri-parts (name rel) (target-type rel))))
                             ;; Generic "something went boom
                             (error () (log-message :error "get-linked-resources failed to fetch something")))))
               rels)))
    ;; This can't be a resource
    (progn
      (log-message :error (format nil "get-linked-resources failed: ~{/~A~} can't be a resource"
                                  uri-parts))
      ;; Return nil
      nil)))

(defun make-simple-alist (lst tag)
  "Turn a list of atoms into a list of '(,tag <atom>) lists"
  (mapcar #'(lambda (atm)
              (list tag atm))
          lst))

(defun search-for-tasks (server &key tags statuses scale urgency importance uid-regex)
  "Search for tasks using the requested parameters.
  Return a plist:
  :uid
  :scale
  :importance
  :urgency
  :status"
  (declare (type rg-server server)
           (type list tags statuses scale urgency importance)
           (type (or null string) uid-regex))
  (log-message :debug (format nil "Searching for tasks with tags ~{~A~^, ~}, importances ~{~A~^, ~}, urgencies ~{~A~^ ~} and statuses ~{~A~^, ~}"
                              (or tags '("<any>"))
                              (or importance '("<any>"))
                              (or urgency '("<any>"))
                              (or statuses '("<any>"))))
  (let ((query-string
          (format nil "/Tasks?~{~A~^&~}"
                  (remove-if
                    #'null
                    (list (when tags (format nil "~{RGoutbound=/TAGS/Tags/~A~^&~}" tags))
                          (when statuses (format nil "status=~{~A~^,~}" statuses))
                          (when scale (format nil "scale=~{~A~^,~}" scale))
                          (when urgency (format nil "urgency=~{~A~^,~}" urgency))
                          (when importance (format nil "importance=~{~A~^,~}" importance))
                          (when uid-regex (format nil "uid=~A" uid-regex)))))))
    (log-message :debug (format nil "Using query URL '~A'" query-string))
    ;; Transform the results into an alist
    (let ((raw-results (mapcar
                         #'(lambda (row)
                             ;; Eliminate WTF sort-order by removing spurious characters
                             ;; that are prone to creeping in, like leading/trailing spaces
                             ;; and quote-marks.
                             (let* ((uid (cdr (assoc :UID row)))
                                    (title (uid-to-title uid))
                                    (scale (cdr (assoc :SCALE row)))
                                    (scale-numeric (cond ((equal "high" scale) 1)
                                                         ((equal "medium" scale) 2)
                                                         (t 3)))
                                    (importance (cdr (assoc :IMPORTANCE row)))
                                    (importance-numeric (cond ((equal "high" importance) 1)
                                                              ((equal "medium" importance) 2)
                                                              (t 3)))
                                    (urgency (cdr (assoc :URGENCY row)))
                                    (urgency-numeric (cond ((equal "high" urgency) 1)
                                                           ((equal "medium" urgency) 2)
                                                           (t 3)))
                                    (status (cdr (assoc :STATUS row)))
                                    ;; Filter out deadlines that were recorded as an empty string
                                    (deadcandidate (cdr (assoc :DEADLINE row)))
                                    (deadline (when (and deadcandidate
                                                         (stringp deadcandidate)
                                                         (not (equal "" deadcandidate)))
                                                deadcandidate)))
                               (list :uid uid
                                     :title title
                                     :scale scale
                                     :scale-numeric scale-numeric 
                                     :importance importance
                                     :importance-numeric importance-numeric
                                     :urgency urgency
                                     :urgency-numeric urgency-numeric
                                     :status status
                                     :deadline deadline)))
                         (rg-request-json server query-string))))
      (log-message :debug (format nil "Raw results of task-search: ~A" raw-results))
      ;; Sort the alist and return it.
      ;; Note that `stable-sort` is required, to prevent each iteration from scrambling its predecessor's results
      (stable-sort
        (stable-sort
          (stable-sort
            (sort raw-results #'> :key #'(lambda (item) (getf item :scale-numeric)))
            #'< :key #'(lambda (item) (getf item :importance-numeric)))
          #'< :key #'(lambda (item) (getf item :urgency-numeric)))
        #'string< :key (lambda (item) (getf item :deadline))))))


(defun get-enum-vals (attr attrlist)
  "Extract the enum values for an attribute,
  from the alist of schema-rtype-attr structs returned by get-attrs-with-keywords.
  It's possible it returns a list of strings."
  (log-message :debug (format nil "Extracting values for '~A' from attribute-list ~A" attr attrlist))
  (schema-rtype-attrs-values (cdr (assoc attr attrlist))))

(defun sanitise-uid (uid)
  "Replace UID-unfriendly characters in UIDs with something safe.
   Expects a string and returns another string."
  (cl-ppcre:regex-replace-all "[/ ]" uid "_"))

(defun get-sub-uri (uri base-uri)
  "Extract the URI from the full request string,
   excluding the base URL and any GET parameters.
   Expects two strings; returns one string."
  (first (cl-ppcre:split "\\?" (cl-ppcre:regex-replace base-uri uri ""))))

(defun filter-params (name params)
  "Filter Hunchentoot parameters for those matching a specific name."
  (remove-if #'(lambda (param)
                 (or (null param)
                     (equal param "")))
             (mapcar #'(lambda (par)
                         (when (equal (car par) name) (cdr par)))
                     params)))
