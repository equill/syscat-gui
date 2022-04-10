;   Copyright 2022 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

(in-package #:webcat-gui)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))

(defun header-clwho (outstr &key (title "Page title") stylesheets javascripts)
  "Convenience function for generating the header section for cl-who.
  outstr = output stream."
  (cl-who:with-html-output
    (outstr)
    (cl-who:htm
      (:head
        :title title
        ;; Stylesheets
        (loop for sheet in (append '("common") stylesheets)
              do (cl-who:htm (:link :rel "stylesheet"
                                    :type "text/css"
                                    :href (format nil "/static/css/~A.css" sheet))))
        ;; Javascript
        (loop for script in (append '("jquery-3.6.0" "common") javascripts)
              do (cl-who:htm (:script :src (format nil "/static/js/~A.js" script))))))))

(defun navbar (outstr &key resourcetype uid)
  "Top-of-page navigation bar.
  outstr = output stream"
  (cl-who:with-html-output
    (outstr nil
      :prologue nil
      :indent t)
    (cl-who:htm
      (:div :class "navbar"
            (:a :class "nav-item-1" :href "/search" "Search")
            ;; Link to the Edit page _if_ we have something to link to
            (if (and resourcetype uid)
              (cl-who:htm (:a :class "nav-item-2"
                              :href (format nil "/editresource/~A/~A" resourcetype uid)
                              "Edit"))
              (cl-who:htm (:div :class "nav-item-2" "Edit")))
            (:a :class "nav-item-3" :href "/create" "Create a new item")
            (:a :class "nav-item-4" :href "/Tasks" "Tasks")
            (:a :class "nav-item-5" :href "/files-upload" "Files")
            (:a :class "nav-item-6" :href "/image-gallery" "Image gallery")))))

(defun default-clwho-layout (outstr &key (title "Page title goes here")
                                    stylesheets
                                    javascripts
                                    resourcetype
                                    uid
                                    content
                                    tags  ; List of UIDs
                                    outbound-links  ; List of (relationships . URI) dotted pairs
                                    )
  "Standard layout setup using cl-who. Content must be wrapped in a (:div) or other similar container."
  (cl-who:with-html-output
    (outstr nil
            :prologue t
            :indent t)
    (:html
      (:head
        (:title (princ title outstr))
        ;; Stylesheets
        (loop for sheet in (append '("common") stylesheets)
              do (cl-who:htm (:link :rel "stylesheet"
                                    :type "text/css"
                                    :href (format nil "/static/css/~A.css" sheet))))
        ;; Javascript
        (loop for script in (append '("jquery-3.6.0" "common") javascripts)
              do (cl-who:htm (:script :src (format nil "/static/js/~A.js" script)))))
      (:body
        (:div
          :class "default-grid"
          (:h1 :id "title" (format outstr "[~A] ~A" resourcetype (uid-to-title uid)))
          (navbar outstr
                  :resourcetype resourcetype
                  :uid uid)
          ;; Body-content
          (princ content outstr)
          ;; Tags
          (:div :id "tags"
                (:h2 :id "tags_header" :class "page-bottom-title" "Tags")
                (:ul :id "taglist" :class "page-bottom-list"
                     (loop for tag in tags
                           do (cl-who:htm (:li (princ (getf tag :uid) outstr))))))
          ;; Outbound links
          (:div :id "outbound-links"
                (:h2 :id "outbound_links" :class "page-bottom-title" "Outbound links")
                (:ul :id "outbound_linklist" :class "page-bottom-list"
                     (loop for link in outbound-links
                           do (cl-who:htm (:li (:a :href (format nil "/display/~A/~A"
                                                                 (getf link :target-type)
                                                                 (getf link :uid))
                                                   (format outstr "~A: /~A/~A"
                                                           (getf link :relationship)
                                                           (getf link :target-type)
                                                           (getf link :uid))))))))
          (:div :id "edit-tags"
                (:a :id "edit-tags" :href (format nil "/edit_links/~A/~A" resourcetype uid)
                    "Edit tags and links")))))))

(defun display-default (outstr attrs)
  "Default content layout.
  attrs arg should be a list of (name . value) dotted pairs,
  where the value will be interpreted by cl-who:with-html-output."
  (cl-who:with-html-output
    (outstr)
    (cl-who:htm
      (:div :class "content-default"
            (:h2 :id "title-attributes" "Item attributes")
            (:table :id "table-attributes"
                    (mapcar #'(lambda (attr)
                                `(:tr
                                   (:td :class "attrnames" ,(car attr))
                                   (:td :class "attrvals" ,(cdr attr))))
                            attrs))))))

(defun sort-linked-resources (resources)
  "Sort the list of resources returned by `get-linked-resources`"
  (unless (every #'(lambda (obj) (typep obj 'linked-resource)) resources)
    (error "This isn't a list of linked-resource instances!"))
  (log-message :debug (format nil "Sorting resource-list ~{~A~^, ~}"
                              (mapcar #'(lambda (res)
                                          (format nil "/~A/~A/~A"
                                                  (relationship res)
                                                  (target-type res)
                                                  (uid res)))
                                      resources)))
  (stable-sort
    (stable-sort
      (sort resources #'string< :key #'uid)
      #'string< :key #'target-type)
    #'string< :key #'relationship))

(defun listify-outbound-links (links filter)
  "Render a list of linked-resource instances into an alist.
  Filter them using `remove-if-not` with the supplied function."
  (mapcar #'(lambda (link)
              (list :RELATIONSHIP (relationship link)
                    :TARGET-TYPE (target-type link)
                    :DEPENDENT-P (dependent-p link)
                    :UID (uid link)))
          (sort-linked-resources (remove-if-not filter links))))

(defun display-item ()
  "Display an item"
  (log-message :debug (format nil "Handling display request from URI ~A" (tbnl:request-uri*)))
  (let* ((uri-parts (get-uri-parts (tbnl:request-uri*) tbnl:*acceptor*))
         (resourcetype (second uri-parts))
         (uid (third uri-parts))
         (content (rg-request-json (rg-server tbnl:*acceptor*)
                                   (format nil "/~A/~A" resourcetype uid)))
         ;; Get a hash-table of attribute definitions
         (attrdefs (get-attrs-with-keywords (rg-server tbnl:*acceptor*) resourcetype)))
    (log-message :debug (format nil "Content: ~A" content))
    (log-message :debug (format nil "Resource-type attributes: ~A" attrdefs))
    (cond
      (content
        ;; Selectively pre-render attribute values, according to their type.
        ;; Only used as the default option, if we didn't find a special-case for this resource-type
        (let ((outbound-links (get-linked-resources (rg-server tbnl:*acceptor*) (cdr uri-parts))))
          (setf (tbnl:content-type*) "text/html")
          (setf (tbnl:return-code*) tbnl:+http-ok+)
          (with-output-to-string (outstr)
            (default-clwho-layout
              outstr
              ;; If it's a wikipage _and_ it has a title, use that.
              ;; Otherwise, just de-url-escape the UID
              :title (or (cdr (assoc :title content))
                         (uid-to-title uid))
              :javascripts '("display")
              :resourcetype resourcetype
              :uid uid
              ;; Additional stylesheets for specific resource-types
              :stylesheets (append
                             '("display")
                             (when (equal "Files" resourcetype)
                               '("files_display"))
                             (when (equal "Tasks" resourcetype)
                               '("tasks_display")))
              :content (cond
                         ;; Display a task
                         ((equal resourcetype "Tasks")
                          (cl-who:with-html-output-to-string
                            (content-string nil
                                            :prologue nil
                                            :indent t)
                            (:div :class "task-grid"
                                  ;; State
                                  (:div :id "current-state-name" :class "taskattr_name" "Current state:")
                                  (:div :id "current-state-value"
                                        :class "taskattr_value"
                                        (princ (or (cdr (assoc :currentstate content)) "(No current-state found)")
                                               content-string))
                                  ;; Next actions
                                  (:div :id "next-actions-name" :class "taskattr_name" "Next actions:")
                                  (:div :id "next-actions-value"
                                        :class "taskattr_value"
                                        (princ (or (cdr (assoc :nextactions content)) "(No next actions found)")
                                               content-string))
                                  ;; Description
                                  (:div :id "task_description"
                                        (princ
                                          (let ((description (cdr (assoc :description content))))
                                            (if description
                                              (3bmd:parse-string-and-print-to-stream description content-string)
                                              "(No description found)")
                                            content-string)))
                                  ;; Importance
                                  (:div :id "importance-name" :class "taskattr_name" "Importance:")
                                  (:div :id "importance-value"
                                        :class "taskattr_value"
                                        (princ (or (cdr (assoc :importance content)) "(No importance found)")
                                          content-string))
                                  ;; Urgency
                                  (:div :id "urgency-name" :class "taskattr_name" "Urgency:")
                                  (:div :id "urgency-value"
                                        :class "taskattr_value"
                                        (princ (or (cdr (assoc :urgency content)) "(No importance found)")
                                          content-string))
                                  ;; Scale
                                  (:div :id "scale-name" :class "taskattr_name" "Scale:")
                                  (:div :id "scale-value"
                                        :class "taskattr_value"
                                        (princ (or (cdr (assoc :scale content)) "(No scale found)")
                                          content-string))
                                  ;; Status
                                  (:div :id "status-name" :class "taskattr_name" "Status:")
                                  (:div :id "status-value"
                                        :class "taskattr_value"
                                        (princ (or (cdr (assoc :status content)) "(No status found)")
                                          content-string)))))
                         ;; Display a wikipage
                         ((equal resourcetype "Wikipages")
                          (cl-who:with-html-output-to-string
                            (content-string nil
                                            :prologue nil
                                            :indent t)
                            (:div :class "content"
                                  (if (cdr (assoc :text content))
                                    (3bmd:parse-string-and-print-to-stream
                                     (cdr (assoc :text content))
                                     content-string)
                                    ""))))
                         ;; Display a file
                         ((equal resourcetype "Files")
                          (cl-who:with-html-output-to-string
                            (content-string nil
                                            :prologue nil
                                            :indent t)
                            (:div :class "task-grid"
                                  ;; Title
                                  (:h1 (princ (cdr (assoc :title content)) content-string))
                                  ;; MIME-type
                                  (:div :id "mimetype-name"
                                        :class "fileattr_name"
                                        "MIME-type")
                                  (:div :id "mimetype-value"
                                        :class "fileattr_value"
                                        (princ (cdr (assoc :mimetype content)) content-string))
                                  ;; File and link
                                  (:div :id "files_image"
                                        (:a :href (format nil "/files/v1/~A" uid)
                                            (:img :id "files_image"
                                                  :src (format nil "/files/v1/~A" uid))))
                                  ;; Notes
                                  (:div :id "notes"
                                        :class "notes"
                                        (if (cdr (assoc :notes content))
                                          (princ (cdr (assoc :notes content)) content-string)
                                          "")))))
                         ;; Default item display
                         (t (cl-who:with-html-output-to-string
                              (content-string nil
                                              :prologue nil
                                              :indent t)
                              (:div
                                :class "content"
                                (:h2 :id "title-attributes" "Item attributes")
                                (:table :id "table-attributes"
                                        (loop for attribute in attrdefs
                                              do
                                              (let* ((attrname (car attribute))
                                                     (attrval (cdr (assoc attrname content))))
                                                (cl-who:htm
                                                  (:tr
                                                    (:td :class "attrnames"
                                                         (princ (schema-rtype-attrs-name (cdr attribute)) content-string))
                                                    (:td :class "attrvals"
                                                         (if attrval
                                                           (3bmd:parse-string-and-print-to-stream
                                                            attrval
                                                            content-string)
                                                           "")))))))))))
              :tags (listify-outbound-links
                      outbound-links
                      #'(lambda (link)
                          (equal (relationship link) "TAGS")))
              :outbound-links (listify-outbound-links
                                outbound-links
                                #'(lambda (link)
                                    (not (equal (relationship link) "TAGS")))))
            :stream outstr)))
      ;; No such resource, but we do have a resourcetype and UID.
      ;; Redirect to the item-creation page.
      ((and resourcetype
            uid)
       (tbnl:redirect (format nil "/create?resourcetype=~A&uid=~A" resourcetype uid)))
      ;; No content, and not enough for constructing a new item: respond with a blank look.
      (t
        (progn
          (setf (tbnl:content-type*) "text/plain")
          (setf (tbnl:return-code*) tbnl:+http-not-found+)
          "No content")))))

(defun edit-links ()
  "Edit a resource's links to tags and other resources."
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
                        (concatenate 'string resource "/TAGS/Tags"))))
            (all-tags (get-uids (rg-server tbnl:*acceptor*) "/Tags")))
       (with-output-to-string (outstr)
         (default-clwho-layout
           outstr
           :title (format nil "Edit tags and links for ~A ~A"
                          resourcetype (uid-to-title resourcename))
           :resourcetype "Edit"
           :uid "links"
           :stylesheets '("edit_links" "display")
           :javascripts '()
           :content
           (cl-who:with-html-output-to-string
             (content-string nil
                             :prologue nil
                             :indent t)
             (:div :class "content-relforms"
                   ;; Form for updating tags
                   (:form :id "tagform"
                          :class "formgrid-edit-tags"
                          :method "post"
                          :action (format nil "/edit_links~A" resource)
                          (:div :id "title-add"
                                (:h3 "Add tags"))
                          (:select :name "add-tags"
                                   :multiple "multiple"
                                   :size 10
                                   :id "tags-add"
                                   ;; List of tags available to be added,
                                   ;; excluding those already associated
                                   (loop for tag in (sort
                                                      (set-difference all-tags extant-tags :test #'equal)
                                                      #'string<)
                                         do (cl-who:htm (:option :value tag (princ tag content-string)))))
                          (:div :id "tags-remove"
                                (:h3 "Remove tags"))
                          (:select :name "remove-tags"
                                   :multiple "multiple"
                                   :size 10
                                   :class "tags-remove"
                                   (loop for tag in (sort extant-tags #'string<)
                                         do (cl-who:htm (:option :value tag
                                                                 (princ tag content-string)))))
                          (:div :id "tags-submit"
                                (:input :type "submit"
                                        :class "submit-button"
                                        :value "Update tags")))
                   ;; Form for adding a relationship
                   (:form :id "relationshipform"
                          :class "formgrid-add-relationship"
                          :method "get"
                          :action (format nil "/create_relationship")
                          (:div :id "title-add-rel"
                                (:h3 "Link to other resources"))
                          (:div :id "rel-input"
                                "URI of target resource:"
                                (:input :id "resource_links"
                                        :name "target"
                                        :type "text"
                                        :value ""))
                          (:input :name "source"
                                  :type "hidden"
                                  :value (format nil "~{/~A~}" (cdr uri-parts)))
                          (:input :type "submit"
                                  :class "rels-submit"
                                  :value "Select relationship"))))))))
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
                      (log-message :debug (format nil "Adding tag ~A" (cdr param)))
                      (multiple-value-bind (body status-code)
                        (rg-post-json (rg-server tbnl:*acceptor*)
                                      (concatenate 'string
                                                   "/" resourcetype "/" uid "/TAGS")
                                      :payload `(("target"
                                                  . ,(concatenate 'string "/Tags/" (cdr param)))))
                        ;; Did it work?
                        (if (or (< status-code 200)
                                (> status-code 299))
                          (push (list :attrname (concatenate 'string
                                                             "Failed to add tag " (cdr param))
                                      :attrval (format nil "~A: ~A" status-code body))
                                update-errors))))
                     ;; Remove a tag
                     ((equal (car param) "remove-tags")
                      (log-message :debug (format nil "Removing tag ~A" (cdr param)))
                      (multiple-value-bind (status-code body)
                        (rg-delete (rg-server tbnl:*acceptor*)
                                   (concatenate 'string
                                                "/" resourcetype "/" uid "/TAGS")
                                   :payload (list (concatenate 'string "target=/Tags/" (cdr param))))
                        ;; Did it work?
                        (if (or (< status-code 200)
                                (> status-code 299))
                          (push (list :attrname (concatenate 'string
                                                             "Failed to remove tag " (cdr param))
                                      :attrval (format nil "~A: ~A" status-code body))
                                update-errors))))
                     ;; FIXME: turn this into a single-target link to /create_relationship
                     ;; Links to other resources
                     ((equal (car param) "resource_links")
                      (log-message :debug (format nil "Linking to resources '~A'" (cdr param)))
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
                                          (log-message
                                            :debug
                                            (format nil "Failed to add link to '~A'. ~A: ~A"
                                                    (cdr param) status-code body))
                                          (push (list
                                                  :attrname (format nil "Failed to add link to '~A'"
                                                                    (cdr param))
                                                  :attrval (format nil "~A: ~A" status-code body))
                                                update-errors))))))
                              ;; Split on any quantity of whitespace
                              (cl-ppcre:split "[ ]+" (cdr param))))
                     ;; Something else
                     (t (log-message :debug (format nil "Other parameter supplied to edit-tags: ~A"
                                                    param)))))
               (tbnl:post-parameters*))
       ;; At least one of those updates broke:
       (if update-errors
         (progn
           (log-message :debug (format nil "Detected update errors: ~A" update-errors))
           (setf (tbnl:content-type*) "text/html")
           (setf (tbnl:return-code*) tbnl:+http-bad-request+)
           (with-output-to-string (outstr)
             (default-clwho-layout
               outstr
               :title (format nil "Failed to create ~A" uid)
               :resourcetype resourcetype
               :uid uid
               :stylesheets '("display")
               :javascripts '("display")
               :content (display-default outstr update-errors)))))
       ;; Happy path: no errors
       (tbnl:redirect (concatenate 'string "/display/" resourcetype "/" uid ))))
    (t (method-not-allowed))))

(defun fetch-available-relationships (rg-server source-resourcetype target-resourcetype)
  "Helper function for fetching the list of available relationships between two resourcetypes."
  (remove-if-not
    #'(lambda (rel)
        (equal target-resourcetype
               (cdr (assoc :target-type rel))))
    (cdr (assoc :relationships
                (rg-request-json rg-server
                                 source-resourcetype
                                 :api "schema")))))

(defun select-relationship ()
  "List the available relationships to the target resource, and provide a means of selecting one."
  (log-message :debug (format nil "Handling ~A request for URI ~A" (tbnl:request-method*) (tbnl:request-uri*)))
  (cond
    ;; GET request, where A source and target resource have been specified
    ((and (equal :GET (tbnl:request-method*))
          (tbnl:get-parameter "source")
          (tbnl:get-parameter "target"))
     (let* ((source-resourcetype
              (car (last (get-uri-parts (tbnl:get-parameter "source") tbnl:*acceptor*) 2)))
            (target-resourcetype
              (car (last (get-uri-parts (tbnl:get-parameter "target") tbnl:*acceptor*) 2)))
            ;; Get a list of alists describing the available _outbound_ relationships
            (available-relationships
              (append
                (fetch-available-relationships (rg-server tbnl:*acceptor*)
                                               source-resourcetype
                                               target-resourcetype)
                (fetch-available-relationships (rg-server tbnl:*acceptor*)
                                               "any"
                                               target-resourcetype)
                (fetch-available-relationships (rg-server tbnl:*acceptor*)
                                               source-resourcetype
                                               "any")))
            ;; Get a list of available relationships _back_ to the source resource,
            ;; so the user can set this up in both directions in one action.
            (available-return-rels
              (append
                (fetch-available-relationships (rg-server tbnl:*acceptor*)
                                               target-resourcetype
                                               source-resourcetype)
                (fetch-available-relationships (rg-server tbnl:*acceptor*)
                                               target-resourcetype
                                               "any")
                (fetch-available-relationships (rg-server tbnl:*acceptor*)
                                               "any"
                                               source-resourcetype))))
       (setf (tbnl:content-type*) "text/html")
       (setf (tbnl:return-code*) tbnl:+http-ok+)
       (with-output-to-string (outstr)
         (default-clwho-layout
           outstr
           :title "Relationship selector"
           :resourcetype "Select"
           :uid "relationship"
           :stylesheets '("display" "select_relationship")
           :javascripts '()
           :content
           (cl-who:with-html-output-to-string
             (content-string nil
                             :prologue nil
                             :indent t)
             (:form
               :id "linkform"
               :class "formgrid-select-relationship"
               :action "/create_relationship"
               :method "post"
               ;; Outbound relationships
               ;; Title
               (:div :id "outbound-title" (:h3 "Outbound relationship:"))
               ;; Relationship types
               (:div :id "available-rels"
                     (:p (format content-string "~A --> ~A: "
                                 (tbnl:get-parameter "source")
                                 (tbnl:get-parameter "target")))
                     (:select :id "relationships"
                              :name "relationship"
                              :size 15
                              (loop for rel in available-relationships
                                    do (let ((name (cdr (assoc :name rel)))
                                             (cardinality (cdr (assoc :cardinality rel))))
                                         (cl-who:htm (:option :value name
                                                              (format content-string
                                                                      "~A (cardinality: ~A)"
                                                                      name cardinality)))))))
               ;; Return relationships
               ;; Title
               (:div :id "return-title" (:h3 "Return relationship:"))
               ;; Relationship types
               (:div :id "available-return"
                     (:p (format content-string "~A <-- ~A: "
                                 (tbnl:get-parameter "source")
                                 (tbnl:get-parameter "target")))
                     (:select :id "return-relationships"
                              :name "return-relationship"
                              :size 15
                              (:option :value ""
                                       :selected "selected"
                                       "(no return relationship)")
                              (loop for rel in available-return-rels
                                    do (let ((name (cdr (assoc :name rel)))
                                             (cardinality (cdr (assoc :cardinality rel))))
                                         (cl-who:htm (:option :value name
                                                              (format content-string
                                                                      "~A (cardinality: ~A)"
                                                                      name cardinality)))))))
               ;; Hidden fields with the information we already have
               (:input :name "source"
                       :type "hidden"
                       :value (tbnl:get-parameter "source"))
               (:input :name "target"
                       :type "hidden"
                       :value (tbnl:get-parameter "target"))
               ;; Submit button
               (:div :id "rels-submit"
                     (:input :id "linkbutton"
                             :type "submit"
                             :value "Create relationship"))))))))
    ;; GET request, where the required parameters have *not* been supplied
    ((equal :GET (tbnl:request-method*))
     (setf (tbnl:content-type*) "text/html")
     (setf (tbnl:return-code*) tbnl:+http-bad-request+)
     "Error page goes here.")
    ;; POST request, where this is actually done, and any constraint-violations are reflected.
    ((equal :POST (tbnl:request-method*))
     ;; Did we get the parameters we needed?
     (if (and (tbnl:post-parameter "source")
              (tbnl:post-parameter "target")
              (tbnl:post-parameter "relationship"))
       ;; Got what we need
       (let* ((source (tbnl:post-parameter "source"))
              (target (tbnl:post-parameter "target"))
              (source-parts
                (remove-if #'(lambda (x)
                               (or (null x)
                                   (equal x "")))
                           (cl-ppcre:split "/" source)))
              (target-parts
                (remove-if #'(lambda (x)
                               (or (null x)
                                   (equal x "")))
                           (cl-ppcre:split "/" target)))
              ;; Outbound relationship components
              (relationship (sanitise-uid (tbnl:post-parameter "relationship")))
              (sourcepath (format nil "~{/~A~}"
                                  (append source-parts
                                          (list relationship))))
              (targetpath (format nil "~{/~A~}" target-parts))
              ;; Grab this here because it gets referred to a lot later on.
              ;; Set it to null if it doesn't have a non-empty value, because we can use that
              ;; to simplify some of the logic below.
              (return-relationship (when (and (tbnl:post-parameter "return-relationship")
                                              (not (equal "" (tbnl:post-parameter
                                                               "return-relationship"))))
                                     (sanitise-uid (tbnl:post-parameter "return-relationship")))))
         ;; Create the relationships
         (log-message :debug (format nil "Linking ~A to ~A" sourcepath target))
         ;; Create the outbound one
         (multiple-value-bind (body status-code)
           (rg-post-json
             (rg-server tbnl:*acceptor*)
             sourcepath
             :payload `(("target" . ,targetpath)))
           ;; Did that work?
           (if (< 199 status-code 300)
             ;; It worked; move on to the reverse case
             ;; First question: was a return relationship requested?
             (if return-relationship
               ;; It was; try to create it.
               (let ((return-sourcepath (format nil "~{/~A~}"
                                                (append target-parts
                                                        (list return-relationship))))
                     (return-targetpath (format nil "~{/~A~}" source-parts)))
                 (log-message
                   :debug
                   (format nil "Outbound relationship successfully created. Now creating return relationship from ~A to ~A"
                           return-sourcepath return-targetpath))
                 (multiple-value-bind (return-body return-status-code)
                   (rg-post-json
                     (rg-server tbnl:*acceptor*)
                     return-sourcepath
                     :payload `(("target" . ,return-targetpath)))
                   ;; Did it work?
                   (if (< 199 return-status-code 300)
                     ;; It did; nothing more to do.
                     (tbnl:redirect (concatenate 'string "/display" source))
                     ;; It failed. Inform the user.
                     (let ((message (format
                                      nil
                                      "Failed to create return relationship ~A from ~A to '~A'. ~A: ~A"
                                      return-relationship 
                                      return-sourcepath
                                      return-targetpath
                                      return-status-code
                                      return-body)))
                       (log-message :debug message)
                       (setf (tbnl:content-type*) "text/html")
                       (setf (tbnl:return-code*) tbnl:+http-bad-request+)
                       (with-output-to-string
                         (outstr)
                         (default-clwho-layout
                           outstr
                           :title "Failed to create return relationship"
                           :resourcetype "Return"
                           :uid "Failed to create"
                           :stylesheets '("display")
                           :javascripts '("display")
                           :content (with-output-to-string (content-string)
                                      (display-default
                                        content-string
                                        `(("Error:" . ,message))))))))))
               ;; No return relationship requested; we're done here.
               (tbnl:redirect (concatenate 'string "/display" source)))
             ;; Failed to create the outbound relationship; stopping here.
             (let ((message (format
                              nil
                              "Failed to create relationship ~A from ~A to '~A'. ~A: ~A"
                              relationship source target status-code body)))
               (log-message :debug message)
               (setf (tbnl:content-type*) "text/html")
               (setf (tbnl:return-code*) tbnl:+http-bad-request+)
               (with-output-to-string
                 (outstr)
                 (default-clwho-layout
                   outstr
                   :title "Failed to create outbound relationship"
                   :resourcetype "Outbound"
                   :uid "Failed to create"
                   :stylesheets '("display")
                   :javascripts '("display")
                   :content (with-output-to-string (content-string)
                              (display-default
                                content-string
                                `(("Error:" . ,message))))))))))
       ;; We didn't get those parameters
       (progn
         (setf (tbnl:content-type*) "text/html")
         (setf (tbnl:return-code*) tbnl:+http-bad-request+)
         "Need both the <code>source</code> and <code>target</code> parameters. Please try again.")))
    ;; Fallback: not implemented
    (t
      (method-not-allowed))))


(defun image-gallery ()
  "Display the image gallery"
  (log-message :debug "Displaying image gallery")
  (let* (;; Get tags currently applied to files.
         ;; Unfortunately it's not feasible to filter specifically for *image* files
         ;; without going directly to the database.
         (tags-available (sort
                           (get-uids (rg-server tbnl:*acceptor*) "/Tags?RGinbound=/Files/*/TAGS")
                           #'string<))
         ;; Get the requested tags
         (tags-requested (filter-params "tags" (tbnl:get-parameters*)))
         ;; Execute the search
         (images (rg-request-json
                   (rg-server tbnl:*acceptor*)
                   (format nil "/Files?mimetype=image/.*~A"
                           ;; Tag-search criterion
                           (if tags-requested
                             (format nil "~{&RGoutbound=/TAGS/Tags/~A~}" tags-requested)
                             "")))))
    (log-message :debug (format nil "Fetched image data ~A" images))
    (with-output-to-string (outstr)
      (default-clwho-layout
        outstr
        :title "Gallery"
        :resourcetype "Images"
        :uid "All"
        :stylesheets '("display" "gallery")
        :javascripts '()
        :content (cl-who:with-html-output-to-string
                   (content-string nil
                                   :prologue nil
                                   :indent t)
                   (:div :class "content"
                         (:div :id "image-list"
                               ;; Search criteria
                               (:div :id "gallery-filter")
                               (:form :action "/image-gallery"
                                      :method "get"
                                      ;; Filter by tags
                                      (:div :class "header" "Tags")
                                      (:select :name "tags"
                                               :size 5
                                               :multiple "multiple"
                                               (loop for tag in tags-available
                                                     do (if (member tag tags-requested :test #'equal)
                                                          (cl-who:htm (:option :value tag
                                                                               :selected "selected"
                                                                               (princ tag content-string)))
                                                          (cl-who:htm (:option :value tag (princ tag content-string)))
                                                          )))
                                      (:div :id "filter-button"
                                            (:input :type "submit" :value "Filter images")))
                               ;; Search results - the actual gallery
                               (loop for img in images
                                     do (cl-who:htm
                                          (:div :class "gallery-image"
                                                (:a :href (format nil "/display/Files/~A" (cdr (assoc :UID img)))
                                                    (:img :class "gallery-image"
                                                          :src (format nil "/files/v1/~A" (cdr (assoc :UID img)))))
                                                (:div :class "gallery-image-text"
                                                      (princ (cdr (assoc :TITLE img)) content-string))))))))))))

(defun edit-resource ()
  "Handle the edit-page for an item"
  (log-message :debug (format nil "Attempting to edit an item with URI ~A" (tbnl:request-uri*)))
  (cond
    ((equal (tbnl:request-method*) :GET)
     (let* ((uri-parts (get-uri-parts (tbnl:request-uri*) tbnl:*acceptor*))
            (resourcetype (second uri-parts))
            (uid (third uri-parts))
            (content (rg-request-json (rg-server tbnl:*acceptor*)
                                      (format nil "/~A/~A" resourcetype uid))))
       (log-message :debug (format nil "Attempting to display the edit page for ~A ~A" resourcetype uid))
       (log-message :debug (format nil "Resourcetype: ~A" resourcetype))
       (log-message :debug (format nil "UID: ~A" uid))
       (log-message :debug (format nil "Content: ~A" content))
       ;; Render the content according to resourcetype
       (if content
         (cond ((equal resourcetype "Wikipages")
                (progn
                  (log-message :debug (format nil "Rendering wikipage ~A" uid))
                  (setf (tbnl:content-type*) "text/html")
                  (setf (tbnl:return-code*) tbnl:+http-ok+)
                  (with-output-to-string (outstr)
                    (default-clwho-layout
                      outstr
                      :title (if (and
                                   (assoc :title content)
                                   (not (equal (cdr (assoc :title content)) "")))
                               (cdr (assoc :title content))
                               (uid-to-title uid))
                      :stylesheets '("edit" "display")
                      :resourcetype "Wikipages"
                      :uid uid
                      :content
                      (cl-who:with-html-output-to-string
                        (content-string nil
                                        :prologue nil
                                        :indent t)
                        (:form :class "formgrid-wikipage"
                               :action (format nil "/editresource/Wikipages/~A" uid)
                               :method "post"
                               ;; Title
                               (:div :class "wikititle-title" "Title:")
                               (:input :class "wikititle-value"
                                       :name "title"
                                       :value (when (cdr (assoc :title content))
                                                (cdr (assoc :title content))))
                               ;; Content
                               (:textarea :id "wikipage-content"
                                          :name "text"
                                          :cols 100
                                          :rows 40
                                          (princ (cdr (assoc :text content)) content-string))
                               ;; Submit button
                               (:div :id "submit-row"
                                     (:input :id "wikipage-submit"
                                             :type "submit"
                                             :value "Update"))))))))
               (t
                 (progn
                   (log-message :debug (format nil "Rendering ~A ~A" resourcetype uid))
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
                                        (log-message
                                          :debug
                                          (format nil "Extracting value for attribute '~A'" attrname))
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
                     (log-message :debug (format nil "Attributes: ~A" attributes-to-display))
                     (setf (tbnl:content-type*) "text/html")
                     (setf (tbnl:return-code*) tbnl:+http-ok+)
                     (with-output-to-string (outstr)
                       (default-clwho-layout
                         outstr
                         :title (format nil "Edit ~A: ~A" resourcetype (uid-to-title uid))
                         :resourcetype resourcetype
                         :uid uid
                         :stylesheets '("edit" "display")
                         :content
                         (cl-who:with-html-output-to-string
                           (content-string nil
                                           :prologue nil
                                           :indent t)
                           (:form :class "formgrid-edititem"
                                  :action (format nil "/editresource/~A/~A" resourcetype uid)
                                  :method "post"
                                  (:h2 :id "title-attributes" "Edit attributes")
                                  (:table :class "attributes"
                                          (loop for attr in attributes-to-display
                                                do (let ((attrname (getf attr :attrname))
                                                         (attrval (getf attr :attrval))
                                                         (attrvals (getf attr :attrvals))
                                                         (textarea (getf attr :textarea)))
                                                     (cl-who:htm
                                                       (:tr
                                                         ;; Attribute name
                                                         (:td :class "attrnames"
                                                              (princ attrname content-string))
                                                         (:td
                                                           ;; There's either a textarea or a list of acceptable values.
                                                           ;; Or a single value.
                                                           (cond
                                                             (textarea
                                                               (cl-who:htm
                                                                 (:textarea :class "attrvals"
                                                                            :name attrname
                                                                            (princ attrval content-string))))
                                                             (attrvals
                                                               (cl-who:htm
                                                                 (:select :class "attrvals"
                                                                          :name attrname
                                                                          (loop for val in attrvals
                                                                                do (if (getf val :selected)
                                                                                     (cl-who:htm
                                                                                       (:option :name (getf val :val)
                                                                                                :selected "true"
                                                                                                (princ (getf val :val)
                                                                                                       content-string)))
                                                                                     (cl-who:htm
                                                                                       (:option :name (getf val :val)
                                                                                                (princ (getf val :val)
                                                                                                       content-string))))))))
                                                             ;; Default to single value
                                                             (t
                                                               (cl-who:htm
                                                                 (:input :class "attrvals"
                                                                         :name attrname
                                                                         :value attrval))))))))))
                                  (:div :id "submit-row"
                                        (:input :id "wikipage-submit"
                                                :type "submit"
                                                :value "Update"))))))))))
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
                          (log-message :debug (format nil "Validating parameter '~A' with value '~A'"
                                                      (car param) (cdr param)))
                          (when (member (car param)
                                        valid-attrnames
                                        :test #'equal)
                            ;(cons (car param) (write-json-string (cdr param)))
                            param))
                      (tbnl:post-parameters*))))
       (log-message :debug (format nil "Processing edit request for ~A ~A" resourcetype uid))
       (log-message :debug (format nil "Validated attributes: ~A" validated-attrs))
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
           (with-output-to-string (outstr)
             (setf (tbnl:content-type*) "text/html")
             (setf (tbnl:return-code*) tbnl:+http-bad-request+)
             (default-clwho-layout
               outstr
               :title (format nil "Failed to create ~A '~A'" resourcetype uid)
               :resourcetype resourcetype
               :uid uid
               :stylesheets '("display")
               :javascripts '("display")
               :content (with-output-to-string (content-string)
                          (display-default
                            content-string
                            `(("Server message" . ,body))))))))))
    ;; Fallback: not by this method
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
            (tags-available (sort (get-uids (rg-server tbnl:*acceptor*) "/Tags") #'string<))
            (tags-requested (filter-params "tags" (tbnl:get-parameters*)))
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
                                                    "RGoutbound=/TAGS/Tags/" par))
                                   tags-requested))
                         (search-criteria (append ()
                                                  (when (tbnl:get-parameter "uid_regex")
                                                    (list (format nil "uid=~A"
                                                                  (tbnl:get-parameter "uid_regex")))))))
                    (progn
                      (log-message :debug (format nil "Searching with criteria '~A'" search-criteria))
                      (search-for-resources (rg-server tbnl:*acceptor*)
                                            (tbnl:get-parameter "resourcetype")
                                            (append tags-requested-formatted
                                                    search-criteria
                                                    requested-attributes)))))
                ;; If no resourcetype was specified, tbnl-formatted-results is NIL:
                ())))
       ;; Debug logging for what we've obtained so far
       (log-message :debug (format nil "Schema: ~A" schema))
       (log-message :debug (format nil "Tags: ~A" tags-available))
       (log-message :debug (format nil "Resourcetype supplied: ~A"
                                   (if
                                     (tbnl:get-parameter "resourcetype")
                                     (tbnl:get-parameter "resourcetype") "none")))
       (log-message :debug (format nil "tbnl-formatted search results: ~A" tbnl-formatted-results))
       (setf (tbnl:content-type*) "text/html")
       (setf (tbnl:return-code*) tbnl:+http-ok+)
       (with-output-to-string (outstr)
         (default-clwho-layout
           outstr
           :title "Webcat search page"
           :resourcetype "Search"
           :uid ""
           :stylesheets '("search" "display")
           :javascripts '("search")
           :content
           (cl-who:with-html-output-to-string
             (content-string nil
                             :prologue nil
                             :indent t)
             (:div :id "searchbox"
                   (:form ;:id "searchform"
                     :class "formgrid-search"
                     :method "get"
                     :action "/search"
                     ;; Resource types
                     (:div :id "resourcetypes"
                           (:h3 "Resource types")
                           (:select :name "resourcetype" :id "resourcetype" :size 15
                                    (loop for rtype in schema
                                          do (if (getf rtype :selected)
                                               (cl-who:htm (:option :value (getf rtype :name)
                                                                    :selected "selected"
                                                                    (princ (getf rtype :name) content-string)))
                                               (cl-who:htm (:option :value (getf rtype :name)
                                                                    (princ (getf rtype :name) content-string)))))))
                     ;; Tags
                     (:div :id "tags"
                           (:h3 "Available tags")
                           (:select :name "tags" :id "tags" :multiple "multiple" :size 15
                                    (loop for tag in tags-available
                                          ;; Check on the fly whether this tag is pre-selected
                                          do (if (member tag tags-requested :test #'equal)
                                               (cl-who:htm (:option :value tag
                                                                    :selected "selected"
                                                                    (princ tag content-string)))
                                               (cl-who:htm (:option :value tag (princ tag content-string)))))))
                     ;; Javascript-updated search refinement
                     (:div :id "refine-title"
                           (:h3 "Refine your search"))
                     (:div :id "attrnames" :class "attrnames"
                           (:div :id "refinesearch" "UID regex"))
                     (:div :id "attrvals" :class "attrvals"
                           (:input :type "text"
                                   :name "uid_regex"
                                   :id "refinesearch-input"
                                   :value (or (tbnl:get-parameter "uid_regex") "")))
                     ;; Submit button
                     (:div :id "submit"
                           (:input :type "submit" :id "searchbutton" :value "Update search")))
                   ;; Search results
                   (:div :id "searchresults"
                         (:h2 "Search results")
                         (:ul :class "results-list"
                              (loop for result in tbnl-formatted-results
                                    do (cl-who:htm
                                         (:li (:a :href (format nil "display/~A/~A"
                                                                (tbnl:get-parameter "resourcetype")
                                                                (getf result :uid))
                                                  (format content-string "/~A/~A"
                                                          (tbnl:get-parameter "resourcetype")
                                                          (getf result :uid))))))))))))))
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
     (log-message :debug (format nil "Requested filename was ~A" (tbnl:post-parameter "name")))
     (log-message :debug (format nil "Original filepath was ~A"
                                 (second (tbnl:post-parameter "file"))))
     (multiple-value-bind (response-body status-code)
       (rg-upload-file
         (rg-server tbnl:*acceptor*)
         `(("name" . ,(tbnl:post-parameter "name"))
           ;; It's already a pathname, so we just pass it on through:
           ("file" . ,(first (tbnl:post-parameter "file")))))
       ;; Expected status code is 201; redirect accordingly.
       (if (equal 201 status-code)
         (tbnl:redirect (concatenate 'string "/display/Files/"
                                     (car (last (cl-ppcre:split "/" response-body)))))
         (tbnl:redirect (format nil "/Files?reason=~A" response-body)))))
    ;; Fail to upload a file
    ((equal (tbnl:request-method*) :POST)
     (tbnl:redirect "/files-upload/?reason=~You didn't meet a single one of the requirements."))
    ;; Display the failed-to-upload page
    ((and
       (equal (tbnl:request-method*) :GET)
       (tbnl:get-parameter "reason"))
     (progn
       (log-message :debug "Displaying the failed-to-upload page")
       (log-message :debug (format nil "Reason for failure: '~A'" (tbnl:get-parameter "reason")))
       (setf (tbnl:content-type*) "text/html")
       (setf (tbnl:return-code*) tbnl:+http-ok+)
       (with-output-to-string (outstr)
         (default-clwho-layout
           outstr
           :title "File upload failed"
           :stylesheets '("upload")
           :resourcetype nil
           :uid nil
           :content (cl-who:with-html-output-to-string
                      (content-string nil
                                      :prologue nil
                                      :indent t)
                      (:div :class "content"
                            (:p "Sorry, but there was a problem with uploading the file.")
                            (:p "Reason: " (princ (tbnl:get-parameter "reason") content-string))))))))
    ;; Display the file-upload form
    ((equal (tbnl:request-method*) :GET)
     (setf (tbnl:content-type*) "text/html")
     (setf (tbnl:return-code*) tbnl:+http-ok+)
     (with-output-to-string (outstr)
       (default-clwho-layout
         outstr
         :title "File upload"
         :stylesheets '("upload" "display")
         :javascripts '("display")
         :resourcetype "Files"
         :uid "Upload"
         :content (cl-who:with-html-output-to-string
                    (content-string nil
                                    :prologue nil
                                    :indent t)
                    (:form :class "uploadform"
                           :action "/files-upload"
                           :method "post"
                           :enctype "multipart/form-data"
                           (:div :id "filename-title" "File name:")
                           (:input :type "text" :name "name" :size 128 :id "filename")
                           (:div :id "filetitle" "File:")
                           (:input :type "file" :name "file" :id "file")
                           (:div :id "submit"
                                 (:input :type "submit" :name "Upload" :value "Upload" :width "100%")))))))
    ;; Delete a file
    ((equal (tbnl:request-method*) :DELETE)
     ())
    ;; Fallback: not by this method
    (t (method-not-allowed))))

(defun tasks ()
  "Display the tasks page"
  (cond
    ((equal (tbnl:request-method*) :GET)
     (let* ((task-attrs (get-attrs-with-keywords (rg-server tbnl:*acceptor*) "Tasks"))
            (statuses-requested (filter-params "status" (tbnl:get-parameters*)))
            (tags-available (sort (get-uids (rg-server *webcat-gui-acceptor*) "/Tags?RGinbound=/Tasks/*/TAGS") #'string<))
            (tags-requested (filter-params "tags" (tbnl:get-parameters*)))
            (scale-requested (filter-params "scale" (tbnl:get-parameters*)))
            (urgency-requested (filter-params "urgency" (tbnl:get-parameters*)))
            (importance-requested (filter-params "importance" (tbnl:get-parameters*)))
            (tbnl-formatted-results
              (search-for-tasks (webcat-gui::rg-server webcat-gui::*webcat-gui-acceptor*)
                                :tags tags-requested
                                :statuses statuses-requested
                                :scale scale-requested
                                :urgency urgency-requested
                                :importance importance-requested
                                :uid-regex (when (tbnl:get-parameter "uid_regex")
                                             (tbnl:get-parameter "uid_regex")))))
       ;; Debug logging for what we've obtained so far
       (log-message :debug (format nil "Attributes: ~A" task-attrs))
       (log-message :debug (format nil "Statuses available: ~A" (get-enum-vals :status task-attrs)))
       (log-message :debug (format nil "Tags: ~A" tags-available))
       (log-message :debug (format nil "tbnl-formatted search results: ~A" tbnl-formatted-results))
       (setf (tbnl:content-type*) "text/html")
       (setf (tbnl:return-code*) tbnl:+http-ok+)
       (with-output-to-string (outstr)
         (default-clwho-layout
           outstr
           :title "Webcat tasks search"
           :stylesheets '("tasks_search")
           :javascripts '("search")
           :resourcetype "Tasks"
           :uid "Search"
           :content
           (cl-who:with-html-output-to-string
             (content-string nil
                             :prologue nil
                             :indent t)
             (:div :id "searchbox"
                   (:form :action "/Tasks"
                          :method "get"
                          :class "formgrid-tasks"
                          :id "searchform"
                          ;; Tags
                          (:div :id "tags-title" "Available tags")
                          (:div :id "tags-list"
                                (:select :name "tags" :multiple "multiple" :size 16
                                         (loop for tag in tags-available
                                               do (if (member tag tags-requested :test #'equal)
                                                    (cl-who:htm (:option :value tag
                                                                         :selected "selected"
                                                                         (princ tag content-string)))
                                                    (cl-who:htm (:option :value tag
                                                                         (princ tag content-string)))))))
                          ;; Other criteria
                          (:div :id "refine-title" "Refine your search")
                          (:div :id "uid-regex-name" :class "criteria-name" "UID regex")
                          (:div :id "uid-regex-value"
                                :class "criteria-value"
                                (:input :type "text"
                                        :name "uid_regex"
                                        :value (or (tbnl:get-parameter "uid_regex") "")))
                          ;; Importance
                          (:div :id "importance-name" :class "criteria-name" "Importance")
                          (:select :id "importance-value"
                                   :name "importance"
                                   :class "criteria-value"
                                   :multiple "multiple"
                                   :size 3
                                   (:option :value "" "Any")
                                   (loop for imp in (get-enum-vals :importance task-attrs)
                                         do (if (equal imp (tbnl:get-parameter "importance"))
                                              (cl-who:htm (:option :value imp
                                                                   :selected "selected"
                                                                   (princ imp content-string)))
                                              (cl-who:htm (:option :value imp
                                                                   (princ imp content-string))))))
                          ;; Urgency
                          (:div :id "urgency-name" :class "criteria-name" "Urgency")
                          (:select :id "urgency-value"
                                   :name "urgency"
                                   :class "criteria-value"
                                   :multiple "multiple"
                                   :size 3
                                   (:option :value "" "Any")
                                   (loop for urge in (get-enum-vals :urgency task-attrs)
                                         do (if (equal urge (tbnl:get-parameter "urgency"))
                                              (cl-who:htm (:option :value urge
                                                                   :selected "selected"
                                                                   (princ urge content-string)))
                                              (cl-who:htm (:option :value urge
                                                                   (princ urge content-string))))))
                          ;; Scale
                          (:div :id "scale-name" :class "criteria-name" "Scale")
                          (:select :id "scale-value"
                                   :name "scale"
                                   :class "criteria-value"
                                   :multiple "multiple"
                                   :size 3
                                   (:option :value "" "Any")
                                   (loop for scale in (get-enum-vals :scale task-attrs)
                                         do (if (equal scale (tbnl:get-parameter "scale"))
                                              (cl-who:htm (:option :value scale
                                                                   :selected "selected"
                                                                   (princ scale content-string)))
                                              (cl-who:htm (:option :value scale
                                                                   (princ scale content-string))))))
                          ;; Status
                          (:div :id "status-name" :class "criteria-name" "Status")
                          (:select :id "status-value"
                                   :name "status"
                                   :class "criteria-value"
                                   :multiple "multiple"
                                   :size 3
                                   (:option :value "" "Any")
                                   (loop for status in (get-enum-vals :status task-attrs)
                                         do (if (equal status (tbnl:get-parameter "status"))
                                              (cl-who:htm (:option :value status
                                                                   :selected "selected"
                                                                   (princ status content-string)))
                                              (cl-who:htm (:option :value status
                                                                   (princ status content-string))))))
                          ;; Submit button
                          (:div :id "submit-row"
                                (:input :type "submit" :value "Search" :id "searchbutton")))
                   ;; Search results
                   (:div :id "searchresults"
                         (:h2 "Search results")
                         (:table :class "results-list"
                                 (:tr
                                   (:th "Status")
                                   (:th "Urgency")
                                   (:th "Importance")
                                   (:th "Scale")
                                   (:th "Title")
                                   (:th "Deadline"))
                                 (loop for result in tbnl-formatted-results
                                       do (cl-who:htm
                                            (:tr
                                              (:td :class "results-list" (princ (getf result :status) content-string))
                                              (:td :class "results-list" (princ (getf result :urgency) content-string))
                                              (:td :class "results-list" (princ (getf result :importance) content-string))
                                              (:td :class "results-list" (princ (getf result :scale) content-string))
                                              (:td :class "results-list"
                                                   (:a :href (format nil "display/Tasks/~A" (getf result :uid))
                                                       (princ (getf result :title) content-string)))
                                              (:td :class "results-list"
                                                   (princ (or (getf result :deadline) "") content-string)))))))))))))
    ;; Fallback: not by this method
    (t (method-not-allowed))))

(defun create-item ()
  "Display the create-item page"
  (cond
    ((equal (tbnl:request-method*) :GET)
     (log-message :debug (format nil "Handling create GET request ~A" (tbnl:request-uri*)))
     (setf (tbnl:content-type*) "text/html")
     (setf (tbnl:return-code*) tbnl:+http-ok+)
     (with-output-to-string (outstr)
       (default-clwho-layout
         outstr
         :title "Create item"
         :stylesheets '("create" "display")
         :javascripts '("search")
         :resourcetype (or (tbnl:get-parameter "resourcetype") "Create")
         :uid (or (tbnl:get-parameter "uid") "")
         :content (cl-who:with-html-output-to-string
                    (content-string nil
                                    :prologue nil
                                    :indent t)
                    (:form :action "/create"
                           :method "post"
                           :class "formgrid-create"
                           :id "createform"
                           ;; UID
                           (:div :id "header-uid" :class "header" "UID/title:")
                           (:input :type "text"
                                   :name "uid"
                                   :id "input-uid"
                                   :value (or (tbnl:get-parameter "uid") ""))
                           ;; Resource-types
                           (:div :class "header" :id "header-resourcetypes" "Resource type")
                           (:select :name "resourcetype"
                                    :id "resourcetype"
                                    :size 15
                                    (loop for rtype in (get-resourcetypes (rg-server tbnl:*acceptor*))
                                          do (if (equal rtype (tbnl:get-parameter "resourcetype"))
                                               (cl-who:htm (:option :value rtype
                                                                    :selected "selected"
                                                                    (princ rtype content-string)))
                                               (cl-who:htm (:option :value rtype
                                                                    (princ rtype content-string))))))
                           ;; Javascript-updated attribute management
                           (:div :class "header" :id "header-attributes" "Attributes")
                           (:div :class "attrnames" :id "attrnames")
                           (:div :class "attrvals" :id "attrvals")
                           ;; Submit button
                           (:input :type "submit" :value "Create" :id "createbutton"))))))
    ((equal (tbnl:request-method*) :POST)
     (let ((uid (tbnl:post-parameter "uid"))
           (resourcetype (tbnl:post-parameter "resourcetype")))
       (log-message :debug (format nil "Handling create POST request ~A" (tbnl:request-uri*)))
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
                                               (log-message
                                                 :debug
                                                 (format nil "Validating parameter ~A" (car param)))
                                               (when (and (not (or (null (cdr param))
                                                                   (equal "" (cdr param))))
                                                          (member (car param) valid-attrnames :test #'equal))
                                                 param))
                                           (tbnl:post-parameters*)))))
                 (log-message :debug (format nil "Validated attributes: ~A" validated-attrs))
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
                     (with-output-to-string (outstr)
                       (setf (tbnl:content-type*) "text/html")
                       (setf (tbnl:return-code*) tbnl:+http-bad-request+)
                       (default-clwho-layout
                         outstr
                         :title (format nil "Failed to create ~A '~A'" resourcetype uid)
                         :resourcetype resourcetype
                         :uid uid
                         :stylesheets '("display")
                         :javascripts '("display")
                         :content (with-output-to-string (content-string)
                                    (display-default
                                      content-string
                                      `(("Server message" . ,body)))))))))))))
    ;; Fallback: not by this method
    (t (method-not-allowed))))
