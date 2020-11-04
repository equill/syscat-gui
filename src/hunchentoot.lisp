;   Copyright 2019 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; The REST API server application

(in-package #:cl-webcat)


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

(defun display-item ()
  "Display an item"
  (log-message :debug "Handling display request from URI ~A" (tbnl:request-uri*))
  (let* ((uri-parts (get-uri-parts (tbnl:request-uri*) tbnl:*acceptor*))
         (resourcetype (second uri-parts))
         (uid (third uri-parts))
         (content (rg-request-json (rg-server tbnl:*acceptor*)
                                   (format nil "/~A/~A" resourcetype uid)))
         ;; Get a hash-table of attribute definitions
         (attrdefs (get-attrs-with-keywords (rg-server tbnl:*acceptor*) resourcetype)))
    (log-message :debug "Content: ~A" content)
    (log-message :debug "Resource-type attributes: ~A" attrdefs)
    (if content
        ;; Selectively pre-render attribute values, according to their type.
        ;; Only used as the default option, if we didn't find a special-case for this resource-type
        (let ((layout-template-path (make-pathname :defaults (template-path tbnl:*acceptor*)
                                                   :type "tmpl"
                                                   :name "display_layout"))
              ;; Don't escape HTML tags in the nested content
              (html-template:*string-modifier* #'cl:identity)
              (outbound-links (get-linked-resources (rg-server tbnl:*acceptor*)
                                                    (cdr uri-parts))))
          (log-message :debug "Path to layout template: ~A" layout-template-path)
          (log-message :debug "State of layout template: ~A" (probe-file layout-template-path))
          (setf (tbnl:content-type*) "text/html")
          (setf (tbnl:return-code*) tbnl:+http-ok+)
          (with-output-to-string (outstr)
            (html-template:fill-and-print-template
              layout-template-path
              (list ;; If it's a wikipage _and_ it has a title, use that.
                ;; Otherwise, just de-url-escape the UID
                :title (or (cdr (assoc :title content))
                           (uid-to-title uid))
                :javascripts '((:script "display"))
                :resourcetype resourcetype
                :uid uid
                ;; Additional stylesheets for specific resource-types
                :stylesheets (append
                               '((:sheet "display"))
                               (when (equal "files" resourcetype)
                                 '((:sheet "files_display")))
                               (when (equal "tasks" resourcetype)
                                 '((:sheet "tasks_display"))))
                :content (cond
                           ;; Display a task
                           ((equal resourcetype "tasks")
                            (with-output-to-string (contstr)
                              (html-template:fill-and-print-template
                                (make-pathname :defaults (template-path tbnl:*acceptor*)
                                               :type "tmpl"
                                               :name "display_task")
                                (list :description (or (cdr (assoc :description content)) "(No description found)")
                                      :importance (or (cdr (assoc :importance content)) "(No importance found)")
                                      :urgency (or (cdr (assoc :urgency content)) "(No urgency found)")
                                      :scale (or (cdr (assoc :scale content)) "(No scale found)")
                                      :status (or (cdr (assoc :status content)) "(No status found)"))
                                :stream contstr)))
                           ;; Display a wikipage
                           ((equal resourcetype "wikipages")
                            (let ((content-layout-path (make-pathname :defaults (template-path tbnl:*acceptor*)
                                                                      :type "tmpl"
                                                                      :name "display_wikipage")))
                              (log-message :debug "Content layout path: ~A" content-layout-path)
                              (log-message :debug "State of content layout template: ~A"
                                           (probe-file content-layout-path))
                              (with-output-to-string (contstr)
                                (html-template:fill-and-print-template
                                  content-layout-path
                                  (list :content
                                        (if (cdr (assoc :text content))
                                          (with-output-to-string (mdstr)
                                            (3bmd:parse-string-and-print-to-stream
                                             (cdr (assoc :text content))
                                             mdstr))
                                          ""))
                                  :stream contstr))))
                           ;; Display a file
                           ((equal resourcetype "files")
                            (with-output-to-string (conststr)
                              (html-template:fill-and-print-template
                                (make-pathname :defaults (template-path tbnl:*acceptor*)
                                               :type "tmpl"
                                               :name "display_files")
                                (list :title  (cdr (assoc :title content))
                                      :mimetype (cdr (assoc :mimetype content))
                                      :image-url (format nil "http://~A:~D/files/v1/~A"
                                                         (rg-server-hostname (rg-server tbnl:*acceptor*))
                                                         (rg-server-port (rg-server tbnl:*acceptor*))
                                                         uid))
                                :stream conststr)))
                           ;; Default item display
                           (t (with-output-to-string (contstr)
                                (html-template:fill-and-print-template
                                  (make-pathname :defaults (template-path tbnl:*acceptor*)
                                                 :type "tmpl"
                                                 :name "display_default")
                                  (list :attributes
                                        (mapcar
                                          #'(lambda (attribute)
                                              ;; Extract the value, using the keyworded version of the attribute-name
                                              (log-message :debug "Extracting value for attribute '~A'" (car attribute))
                                              (let* ((attrname (car attribute))
                                                     (attrval (cdr (assoc attrname content))))
                                                ;; Conditionally render the type
                                                (list :attrname (schema-rtype-attrs-name (cdr attribute))
                                                      ; Ensure all values are strings, for the template.
                                                      :attrval (if attrval
                                                                   ;; Render all descriptions as Markdown
                                                                   (if (or (member attrname '(:description :text))
                                                                           (equal "commonmark"
                                                                                  (schema-rtype-attrs-valuetype
                                                                                    (cdr attribute))))
                                                                       (with-output-to-string (mdstr)
                                                                         (3bmd:parse-string-and-print-to-stream attrval mdstr)
                                                                         mdstr)
                                                                       attrval)
                                                                   ""))))
                                          attrdefs))
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

(defun image-gallery ()
  "Display the image gallery"
  (log-message :debug "Displaying image gallery")
  (let* ((layout-template-path (merge-pathnames "display_layout.tmpl" (template-path tbnl:*acceptor*)))
         (gallery-template-path (merge-pathnames "display_gallery.tmpl" (template-path tbnl:*acceptor*)))
         ;; Don't escape HTML tags in the nested content
         (html-template:*string-modifier* #'cl:identity)
         ;; Get available tags
         (tags-available (sort (get-uids (rg-server tbnl:*acceptor*) "tags") #'string<))
         ;; Get the requested tags
         (tags-requested (remove-if #'null
                                    (mapcar #'(lambda (par)
                                                (when (equal (car par) "tags") (cdr par)))
                                            (tbnl:get-parameters*))))
         ;; Execute the search
         (images (rg-request-json
                   (rg-server tbnl:*acceptor*)
                   (format nil "/files?mimetype=image/.*~A"
                           ;; Tag-search criterion
                           (if tags-requested
                             (format nil "~{&outbound=/Tags/tags/~A~}" tags-requested)
                             "")))))
    (log-message :debug "Fetched image data ~A" images)
    (log-message :debug "State of layout template ~A is ~A"
                 layout-template-path (probe-file layout-template-path))
    (log-message :debug "State of gallery template ~A is ~A"
                 gallery-template-path (probe-file gallery-template-path))
    (with-output-to-string (outstr)
      (html-template:fill-and-print-template
        layout-template-path
        (list :title "Gallery"
              :javascripts nil
              :resourcetype "Images"
              :uid "All"
              :stylesheets '((:sheet "display")
                             (:sheet "gallery"))
              :content (with-output-to-string (contstr)
                         (html-template:fill-and-print-template
                           gallery-template-path
                           (list :images
                                 (mapcar
                                   #'(lambda (img)
                                       (list :url (format nil "http://~A:~A/files/v1/~A"
                                                          (rg-server-hostname (rg-server tbnl:*acceptor*))
                                                          (rg-server-port (rg-server tbnl:*acceptor*))
                                                          (cdr (assoc :UID img)))
                                             :link (format nil "/display/files/~A"
                                                           (cdr (assoc :UID img)))
                                             :title (cdr (assoc :TITLE img))))
                                   images)
                                 :tags (mapcar #'(lambda (tag)
                                                   (list :tag tag
                                                         :selected (when (member tag tags-requested :test #'equal)
                                                                     "selected")))
                                               tags-available))
                           :stream contstr)))
        :stream outstr))))

(defun edit-resource ()
  "Handle the edit-page for an item"
  (log-message :debug "Attempting to edit an item with URI ~A" (tbnl:request-uri*))
  (cond
    ((equal (tbnl:request-method*) :GET)
     (let* ((uri-parts (get-uri-parts (tbnl:request-uri*) tbnl:*acceptor*))
            (resourcetype (second uri-parts))
            (uid (third uri-parts))
            (content (rg-request-json (rg-server tbnl:*acceptor*)
                                      (format nil "/~A/~A" resourcetype uid))))
       (log-message :debug "Attempting to display the edit page for ~A ~A" resourcetype uid)
       (log-message :debug "Resourcetype: ~A" resourcetype)
       (log-message :debug "UID: ~A" uid)
       (log-message :debug "Content: ~A" content)
       ;; Render the content according to resourcetype
       (if content
         (cond ((equal resourcetype "wikipages")
                (progn
                  (log-message :debug "Rendering wikipage ~A" uid)
                  (setf (tbnl:content-type*) "text/html")
                  (setf (tbnl:return-code*) tbnl:+http-ok+)
                  (with-output-to-string (outstr)
                    (html-template:fill-and-print-template
                      (make-pathname :defaults (template-path tbnl:*acceptor*)
                                     :type "tmpl"
                                     :name "edit_wikipage")
                      (list :title (if (and
                                         (assoc :title content)
                                         (not (equal (cdr (assoc :title content)) "")))
                                     (cdr (assoc :title content))
                                     (uid-to-title uid))
                            :stylesheets '((:sheet "edit"))
                            :uid uid
                            :content (cdr (assoc :text content)))
                      :stream outstr))))
               (t
                 (progn
                   (log-message :debug "Rendering ~A ~A" resourcetype uid)
                   ;; Render the attributes for editing
                   (let ((attributes-to-display
                           (mapcar
                             #'(lambda (attribute)
                                 ;; Memoise this to simplify the following code
                                 (let ((attrname (schema-rtype-attrs-name attribute)))
                                   ;; Handle differently according to attribute type
                                   (cond
                                     ;; If the `values` attribute is non-null:
                                     ((not (null (schema-rtype-attrs-values attribute)))
                                      (let ((existing-value
                                              (or (cdr
                                                    (assoc
                                                      (intern (string-upcase attrname) 'keyword)
                                                      content))
                                                  "")))
                                        (log-message :debug "Extracting value for attribute '~A'" attrname)
                                        (log-message :debug (format nil "Existing value of ~A: ~A"
                                                                    attrname existing-value))
                                        (list :attrname attrname
                                              :attrvals
                                              (when (schema-rtype-attrs-values attribute)
                                                (mapcar
                                                  #'(lambda (val)
                                                      (list :val val
                                                            :selected (when (equal existing-value val) t)))
                                                  (schema-rtype-attrs-values attribute)))
                                              :textarea nil)))
                                     ;; Default style
                                     (t
                                       (list :attrname attrname
                                             :attrval (or (cdr (assoc
                                                                 (intern (string-upcase attrname) 'keyword)
                                                                 content))
                                                          "")
                                             :attrvals nil
                                             :textarea (member attrname '("description" "text")
                                                               :test #'equal))))))
                             ;; Retrieve the attribute-definitions for this resourcetype
                             (get-attrs (rg-server tbnl:*acceptor*) resourcetype))))
                     (log-message :debug "Attributes: ~A" attributes-to-display)
                     (setf (tbnl:content-type*) "text/html")
                     (setf (tbnl:return-code*) tbnl:+http-ok+)
                     (with-output-to-string (outstr)
                       (html-template:fill-and-print-template
                         (make-pathname :defaults (template-path tbnl:*acceptor*)
                                        :type "tmpl"
                                        :name "edit_resource")
                         (list :resourcetype resourcetype
                               :uid uid
                               :title (format nil "Edit ~A: ~A" resourcetype (uid-to-title uid))
                               :stylesheets '((:sheet "edit"))
                               :attributes attributes-to-display)
                         :stream outstr))))))
         ;; Content not retrieved
         (progn
           (setf (tbnl:content-type*) "text/plain")
           (setf (tbnl:return-code*) tbnl:+http-not-found+)
           "No content"))))
    ;;; POST request: update the resource
    ((equal (tbnl:request-method*) :POST)
     (let* ((uri-parts (get-uri-parts (tbnl:request-uri*) tbnl:*acceptor*))
            (resourcetype (second uri-parts))
            (uid (third uri-parts))
            ;; Extract attributes relevant to this resourcetype
            (valid-attrnames
              (mapcar #'schema-rtype-attrs-name
                      (get-attrs (rg-server tbnl:*acceptor*) resourcetype)))
            (validated-attrs
              (mapcar #'(lambda (param)
                          (log-message :debug "Validating parameter ~A" (car param))
                          (when (member (car param) valid-attrnames :test #'equal) param))
                      (tbnl:post-parameters*))))
       (log-message :debug (format nil "Processing edit request for ~A ~A" resourcetype uid))
       (log-message :debug "Validated attributes: ~A" validated-attrs)
       ;; Send the update
       (multiple-value-bind (body status-code)
         (rg-post-json (rg-server tbnl:*acceptor*)
                       (concatenate 'string "/" resourcetype "/" uid)
                       :payload validated-attrs
                       ;:payload (tbnl:post-parameters*)
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
                 (make-pathname :defaults (template-path tbnl:*acceptor*)
                                :type "tmpl"
                                :name "display_layout")
                 `(:resourcetype :title ,(format nil "Failed to create ~A" uid)
                                 :stylesheets '((:sheet "display"))
                                 :javascripts '((:script "display"))
                                 ,resourcetype
                                 :uid ,uid
                                 :content ,(with-output-to-string (contstr)
                                             (html-template:fill-and-print-template
                                               (make-pathname :defaults (template-path tbnl:*acceptor*)
                                                              :type "tmpl"
                                                              :name "display_default")
                                               `(:attributes ((:attrname "Server message"
                                                                         :attrval ,body)))
                                               :stream contstr)))
                 :stream outstr)))))))
    ;; Fallback: not by this method
    (t (method-not-allowed))))

(defun edit-links ()
  "Edit a resource's links to tags, groups and other resources."
  (cond
    ((equal (tbnl:request-method*) :GET)
     (let* ((uri-parts (get-uri-parts (tbnl:request-uri*) tbnl:*acceptor*))
            (resourcetype (second uri-parts))
            (resourcename (third uri-parts))
            (resource (concatenate 'string "/" resourcetype "/" resourcename))
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
           (make-pathname :defaults (template-path tbnl:*acceptor*)
                          :type "tmpl"
                          :name "edit_links")
           (list :title (format nil "Edit tags, groups and links for ~A ~A"
                                resourcetype (uid-to-title resourcename))
                 :stylesheets '((:sheet "edit_links"))
                 :resource resource
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
                                      (when (or (< status-code 200)
                                                (> status-code 299))
                                        ;; If not, add the error to the error-list
                                        (progn
                                          (log-message :debug "Failed to add link to '~A'. ~A: ~A"
                                                       (cdr param) status-code body)
                                          (push (list :attrname (format nil "Failed to add link to '~A'" (cdr param))
                                                      :attrval (format nil "~A: ~A" status-code body))
                                                update-errors))))))
                              ;; Split on any quantity of whitespace
                              (cl-ppcre:split "[ ]+" (cdr param))))
                     ;; Something else
                     (t (log-message :debug "Other parameter supplied to edit-tags: ~A" param))))
               (tbnl:post-parameters*))
       ;; At least one of those updates broke:
       (if update-errors
         (let ((html-template:*string-modifier* #'cl:identity))
           (log-message :debug "Detected update errors: ~A" update-errors)
           (setf (tbnl:content-type*) "text/html")
           (setf (tbnl:return-code*) tbnl:+http-bad-request+)
           (with-output-to-string (outstr)
             (html-template:fill-and-print-template
               (make-pathname :defaults (template-path tbnl:*acceptor*)
                              :type "tmpl"
                              :name "display_layout")
               `(:resourcetype ,resourcetype
                               :uid ,uid
                               :stylesheets ((:sheet "display"))
                               :javascripts ((:script "display"))
                               :title ,(format nil "Failed to create ~A" uid)
                               :content ,(with-output-to-string (contstr)
                                           (html-template:fill-and-print-template
                                             (make-pathname :defaults (template-path tbnl:*acceptor*)
                                                            :type "tmpl"
                                                            :name "display_default")
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
     (log-message :debug "Displaying the search page.")
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
                                              (mapcar #'schema-rtype-attrs-name
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
           (make-pathname :defaults (template-path tbnl:*acceptor*)
                          :type "tmpl"
                          :name "display_search")
           (list :title "Webcat search page"
                 :stylesheets '((:sheet "search"))
                 :javascripts '((:script "search"))
                 :schema schema
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

(defun files ()
  "GUI for uploading files.
   Does not provide for fetching an existing one; for that, use RG's /files API."
  (cond
    ;; Upload a file
    ;; On success, redirect to the item-view page.
    ((and (equal (tbnl:request-method*) :POST)
          (tbnl:post-parameter "file")
          (tbnl:post-parameter "name"))
     (log-message :debug "Received a file-upload attempt.")
     (log-message :debug "Requested filename was ~A" (tbnl:post-parameter "name"))
     (log-message :debug "Original filepath was ~A" (tbnl:post-parameter "file"))
     (multiple-value-bind (response-body status-code)
       (rg-post-json
         (rg-server tbnl:*acceptor*)
         "/files/v1"
         :payload `(("name" . ,(tbnl:post-parameter "name"))
                    ;; It's already a pathname, so we just pass it on through:
                    ("file" . ,(first (tbnl:post-parameter "file"))))
         :api "files")
       ;; Expected status code is 201; redirect accordingly.
       (if (equal 201 status-code)
           (tbnl:redirect (concatenate 'string "/display/files/"
                                       (car (last (cl-ppcre:split "/" response-body)))))
           (tbnl:redirect (format nil "/files?reason=~A" response-body)))))
    ;; Fail to upload a file
    ((equal (tbnl:request-method*) :POST)
     (tbnl:redirect "/files/?reason=~You didn't meet a single one of the requirements."))
    ;; Display the failed-to-upload page
    ((and
       (equal (tbnl:request-method*) :GET)
       (tbnl:get-parameter "reason"))
     (progn
       (log-message :debug "Displaying the failed-to-upload page")
       (log-message :debug "Reason for failure: '~A'" (tbnl:get-parameter "reason"))
       (setf (tbnl:content-type*) "text/html")
       (setf (tbnl:return-code*) tbnl:+http-ok+)
       (with-output-to-string (outstr)
         (html-template:fill-and-print-template
           (make-pathname :defaults (template-path tbnl:*acceptor*)
                          :type "tmpl"
                          :name "display_fileupload_failed")
           (list :title "File upload failed"
                 :stylesheets '((:sheet "upload"))
                 :reason (tbnl:get-parameter "reason")
                 :resourcetype nil
                 :uid nil)
           :stream outstr))))
    ;; Display the file-upload form
    ((equal (tbnl:request-method*) :GET)
     (setf (tbnl:content-type*) "text/html")
     (setf (tbnl:return-code*) tbnl:+http-ok+)
     (with-output-to-string (outstr)
       (html-template:fill-and-print-template
         (make-pathname :defaults (template-path tbnl:*acceptor*)
                        :type "tmpl"
                        :name "display_fileupload")
         (list :title "File upload"
               :stylesheets '((:sheet "upload")
                              (:sheet "display"))
               :javascripts '((:script "display"))
               :resourcetype nil
               :uid nil)
         :stream outstr)))
    ;; Delete a file
    ((equal (tbnl:request-method*) :DELETE)
     ())
    ;; Fallback: not by this method
    (t (method-not-allowed))))

(defun tasks ()
  "Display the tasks page"
  (cond
    ((equal (tbnl:request-method*) :GET)
     (let* ((task-attrs (get-attrs-with-keywords (rg-server tbnl:*acceptor*) "tasks"))
            (statuses-requested (filter-params "status" (tbnl:get-parameters*)))
            (tags-available (get-task-tags (cl-webcat::neo4j-server cl-webcat::*cl-webcat-acceptor*)))
            (tags-requested (filter-params "tags" (tbnl:get-parameters*)))
            (scale-requested (filter-params "scale" (tbnl:get-parameters*)))
            (urgency-requested (filter-params "urgency" (tbnl:get-parameters*)))
            (importance-requested (filter-params "importance" (tbnl:get-parameters*)))
            (tbnl-formatted-results
              (search-for-tasks (cl-webcat::neo4j-server cl-webcat::*cl-webcat-acceptor*)
                                tags-requested
                                :statuses statuses-requested
                                :scale scale-requested
                                :urgency urgency-requested
                                :importance importance-requested)))
       ;; Debug logging for what we've obtained so far
       (log-message :debug "Attributes: ~A" task-attrs)
       (log-message :debug "Statuses available: ~A" (get-enum-vals :status task-attrs))
       (log-message :debug "Tags: ~A" tags-available)
       (log-message :debug "tbnl-formatted search results: ~A" tbnl-formatted-results)
       (setf (tbnl:content-type*) "text/html")
       (setf (tbnl:return-code*) tbnl:+http-ok+)
       (with-output-to-string (outstr)
         (html-template:fill-and-print-template
           (make-pathname :defaults (template-path tbnl:*acceptor*)
                          :type "tmpl"
                          :name "display_tasks_search")
           (list :stylesheets '((:sheet "tasks_search"))
                 :javascripts '((:script "search"))
                 :title "Webcat tasks search"
                 :statuses (mapcar
                             #'(lambda (status)
                                 (list :name status
                                       :selected (when (member status statuses-requested :test #'equal)
                                                   "selected")))
                             (get-enum-vals :status task-attrs))
                 :importance (mapcar
                               #'(lambda (imp)
                                   (list :name imp
                                         :selected (when (equal imp (tbnl:get-parameter "importance")) "selected")))
                               (get-enum-vals :importance task-attrs))
                 :urgency (mapcar
                            #'(lambda (urge)
                                (list :name urge
                                      :selected (when (equal urge (tbnl:get-parameter "urgency")) "selected")))
                            (get-enum-vals :urgency task-attrs))
                 :scale (mapcar
                          #'(lambda (scale)
                              (list :name scale
                                    :selected (when (equal scale (tbnl:get-parameter "scale")) "selected")))
                          (get-enum-vals :scale task-attrs))
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
     (log-message :debug "Handling create GET request ~A" (tbnl:request-uri*))
     (let ((schema (mapcar #'(lambda (rtype)
                               (list :name rtype :selected nil))
                           (get-resourcetypes (rg-server tbnl:*acceptor*)))))
       (log-message :debug "Retrieved schema data.")
       (setf (tbnl:content-type*) "text/html")
       (setf (tbnl:return-code*) tbnl:+http-ok+)
       (with-output-to-string (outstr)
         (html-template:fill-and-print-template
           (make-pathname :defaults (template-path tbnl:*acceptor*)
                          :type "tmpl"
                          :name "display_createitem")
           (list :title "Create item"
                 :stylesheets '((:sheet "create"))
                 :javascripts '((:script "search"))
                 :schema schema)
           :stream outstr))))
    ((equal (tbnl:request-method*) :POST)
     (let ((uid (tbnl:post-parameter "uid"))
           (resourcetype (tbnl:post-parameter "resourcetype")))
       (log-message :debug "Handling create POST request ~A" (tbnl:request-uri*))
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
               ;; Extract non-empty attributes relevant to this resourcetype
               (let* ((valid-attrnames
                        (mapcar #'schema-rtype-attrs-name
                                (get-attrs (rg-server tbnl:*acceptor*) resourcetype)))
                      (validated-attrs
                        (remove-if #'null
                                   (mapcar #'(lambda (param)
                                               (log-message :debug "Validating parameter ~A" (car param))
                                               (when (and (not (or (null (cdr param))
                                                                   (equal "" (cdr param))))
                                                          (member (car param) valid-attrnames :test #'equal))
                                                 param))
                                           (tbnl:post-parameters*)))))
                 (log-message :debug "Validated attributes: ~A" validated-attrs)
                 ;; Send the update
                 (multiple-value-bind (body status-code)
                   (rg-post-json (rg-server tbnl:*acceptor*)
                                 (concatenate 'string "/" resourcetype)
                                 :payload (append `(("uid" . ,(sanitise-uid uid))) validated-attrs))
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
                           (make-pathname :defaults (template-path tbnl:*acceptor*)
                                          :type "tmpl"
                                          :name "display_layout")
                           `(:resourcetype ,resourcetype
                                           :uid ,uid
                                           :stylesheets '((:sheet "display"))
                                           :javascripts '((:script "display"))
                                           :title ,(format nil "Failed to create ~A" uid)
                                           :content ,(with-output-to-string (contstr)
                                                       (html-template:fill-and-print-template
                                                         (make-pathname
                                                           :defaults (template-path tbnl:*acceptor*)
                                                           :type "tmpl"
                                                           :name "display_default")
                                                         `(:attributes ((:attrname "Server message"
                                                                                   :attrval ,body)))
                                                         :stream contstr)))
                           :stream outstr))))))))))
    ;; Fallback: not by this method
    (t (method-not-allowed))))


;; Appserver startup/shutdown

(defun startup (&key acceptor docker)
  "Start up the appserver.
  Ensures the uniqueness constraint on resource-types is present in Neo4j.
  Keyword arguments:
  - acceptor = prebuilt acceptor, to use instead of the default.
  - dispatchers = extra dispatchers to add to tbnl:*dispatch-table* in addition to the defaults.
  - docker = whether to start up in a manner suitable to running under docker,
  i.e. return only after Hunchentoot shuts down, instead of immediately after it starts up."
  (declare (type (boolean) docker)
           ;; Specialising on acceptor doesn't allow for a null value
           ;(type (or nil cl-webcat-acceptor) acceptor)
           )
  (log-message :info "Attempting to start up the cl-webcat application server")
  ;; Control the decoding of JSON identifiers
  (setf json:*json-identifier-name-to-lisp* 'common-lisp:string-upcase)
  ;; Sanity-check: is an acceptor already running?
  ;;; We can't directly check whether this acceptor is running,
  ;;; so we're using the existence of its special variable as a proxy.
  (if (boundp '*cl-webcat-acceptor*)
    ;; There's an acceptor already in play; bail out.
    (log-message :critical "Acceptor already exists; refusing to create a new one.")
    ;; No existing acceptor; we're good to go.
    (let ((myacceptor (or acceptor (make-acceptor))))
      ;; Set the template path for html-template's includes
      (setf html-template:*default-template-pathname* (template-path myacceptor))
      (log-message :debug "Value of html-template:*default-template-pathname*: ~A"
                   html-template:*default-template-pathname*)
      ;; Make it available as a dynamic variable, for shutdown to work on
      (defparameter *cl-webcat-acceptor* myacceptor)
      ;; Stop html-template raising a warning every time it compiles a template
      (setf html-template:*warn-on-creation* nil)
      ;; Set the dispatch table
      (log-message :info "Configuring the dispatch table")
      (setf tbnl:*dispatch-table*
            (list
              ;; Include the additional dispatchers here
              ;; in alphabetical order, to make it easier to keep track of conflicts
              (tbnl:create-regex-dispatcher "/create$" 'create-item)
              (tbnl:create-prefix-dispatcher "/display" 'display-item)
              (tbnl:create-prefix-dispatcher "/edit_links" 'edit-links)
              (tbnl:create-prefix-dispatcher "/editresource" 'edit-resource)
              (tbnl:create-prefix-dispatcher "/files" 'files)
              (tbnl:create-prefix-dispatcher "/image-gallery" 'image-gallery)
              (tbnl:create-prefix-dispatcher "/healthcheck" 'healthcheck)
              (tbnl:create-prefix-dispatcher "/search" 'searchpage)
              (tbnl:create-folder-dispatcher-and-handler "/static/css/"
                                                         (merge-pathnames "css/" (static-path myacceptor))
                                                         "text/css")
              (tbnl:create-folder-dispatcher-and-handler "/static/js/"
                                                         (merge-pathnames "js/" (static-path myacceptor))
                                                         "text/javascript")
              (tbnl:create-prefix-dispatcher "/tasks" 'tasks)
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
