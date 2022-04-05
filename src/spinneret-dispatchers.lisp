;   Copyright 2022 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

(in-package #:webcat-gui)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))

(defun header (&key (title "Page title") stylesheets javascripts)
  "Convenience function for generating the header section for Spinneret:with-html"
  (append
    `(:head
       :title ,title
       ;; Stylesheets
       (:link :rel "stylesheet" :type "text/css" :href "/static/css/common.css")
       ;; Javascript loading
       (:script :src "/static/js/jquery-3.6.0.js")
       (:script :src "/static/js/common.js"))
    ;; Add the case-specific stylesheets
    (mapcar #'(lambda (sheet)
                `(:link :rel "stylesheet"
                  :type "text/css"
                  :href ,(format nil "/static/css/~A.css" sheet)))
            stylesheets)
    ;; Add the case-specific javascripts
    (mapcar #'(lambda (script)
                `(:script :src ,(format nil "/static/js/~A.js" script)))
            javascripts)))

(defun navbar-top (&key resourcetype uid)
  "Top-of-page navigation bar.
  Needs to be incorporated via spinneret:interpet-html-tree."
  ;; For some reason, :div#navigation is not handled by spinneret:interpret-html-tree
  `(:div :id "navigation"
     (:a :class "nav-item-1" :href "/search" "Search")
     ;; Link to the Edit page _if_ we have something to link to
     ,(if (and resourcetype uid)
          `(:a :class "nav-item-2" :href (format nil "/editresource/~A/uid" resourcetype uid) "Edit")
          '(:div :class "nav-item-2" "Edit"))
     (:a :class "nav-item-3" :href "/create" "Create a new item")
     (:a :class "nav-item-4" :href "/Tasks" "Tasks")
     (:a :class "nav-item-5" :href "/files-upload" "Files")
     (:a :class "nav-item-6" :href="/image-gallery" "Image gallery")))

(defun default-layout (&key (title "Page title goes here")
                            stylesheets
                            javascripts
                            resourcetype
                            uid
                            content
                            tags  ; List of UIDs
                            outbound-links  ; List of (relationships . URI) dotted pairs
                            )
  "Standard layout setup.
   Content must be wrapped in a (:div) or other similar container."
  `(:html
     ,(header :title title
              :stylesheets stylesheets
              :javascripts javascripts)
     (:div#pagegrid
       (:h1#title ,(format nil "~A - ~A" resourcetype uid))
       ,(navbar-top)
       ;; Body-content
       ,content
       ;; Tags
       (:h2#tags_header.page-bottom-title)
       (:ul#taglist.page-bottom-list
         ,(mapcar #'(lambda (tag)
                      `(:li ,tag))
                  tags))
       ;; Outbound links
       (:h2#outbound_links.page-bottom-title "Outbound links")
       (:ul#outbound_linklist.page-bottom-list
         ,(mapcar #'(lambda (link)
                      `(:a :href ,(format nil "/display~A" (cdr link))
                        ,(format nil "~A~A" (car link) (cdr link))))
                  outbound-links))
       (:div#taglink
         (:a#taglink :href (format nil "/edit_links/~A/~A" resourcetype uid)
          "Edit tags and links")))))

(defun display-default (attrs)
  "Default content layout.
   attrs arg should be a list of alists, with keys :attrname and :attrval."
  `(:div :class "content-default"
     (:h2 :id "title-attributes" "Item attributes")
     (:table :id "table-attributes"
       ,(mapcar #'(lambda (attr)
                    `(:tr
                       (:td :class "attrnames" ,(car attr))
                       (:td :class "attrvals" ,(cdr attr))))
                attrs))))

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
            ;; Get a list of alists describing the relationships
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
                                               "any"))))
       (setf (tbnl:content-type*) "text/html")
       (setf (tbnl:return-code*) tbnl:+http-ok+)
       (with-output-to-string (outstr)
         ;; Hacky workaround required by Spinneret
         (let ((*standard-output* outstr))
           (spinneret:with-html
             (:doctype)
             (:html
               ;; Dynamically-generate the HEAD section
               (spinneret:interpret-html-tree
                 (header :title "Relationship selector"
                         :stylesheets '()
                         :javascripts '()))
               (:body
                 (:div#pagegrid
                   (:h1 "Select relationship")
                   (navbar-top)
                   (:form#linkform.formgrid
                     :action "/create_relationship" :method "post"
                     ;; Source resourcetype
                     (:div#header-sourcetype.header "Source resource: ")
                     (:div (format nil "~A of resourcetype ~A"
                                   (tbnl:get-parameter "source")
                                   source-resourcetype))
                     ;; Relationship types
                     (:div#header-relationships.header
                       (format nil "Available relationships from ~A to ~A: "
                               source-resourcetype target-resourcetype))
                     (:select#relationships
                       :name "relationship"
                       :size 15
                       (mapcar #'(lambda (rel)
                                   (let ((name (cdr (assoc :name rel)))
                                         (cardinality (cdr (assoc :cardinality rel))))
                                     (:option :value name
                                              (format nil "~A (cardinality: ~A)" name cardinality))))
                               available-relationships))
                     ;; Hidden fields with the information we already have
                     (:input :name "source"
                              :type "hidden"
                              :value (tbnl:get-parameter "source"))
                     (:input :name "target"
                              :type "hidden"
                              :value (tbnl:get-parameter "target"))
                     ;; Submit button
                     (:input#linkbutton :type "submit" :value "Create relationship"))))))))))
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
                (relationship (sanitise-uid (tbnl:post-parameter "relationship")))
                (target-parts
                  (remove-if #'(lambda (x)
                                 (or (null x)
                                     (equal x "")))
                             (cl-ppcre:split "/" target)))
                (sourcepath (concatenate 'string source "/" (sanitise-uid relationship)))
                (targetpath (format nil "~{/~A~}" target-parts)))
           (log-message :debug (format nil "Linking ~A to ~A" sourcepath target))
           (multiple-value-bind (body status-code)
             (rg-post-json
               (rg-server tbnl:*acceptor*)
               sourcepath
               :payload `(("target" . ,targetpath)))
             ;; Did it work?
             (if (or (< status-code 200)
                     (> status-code 299))
                 (let ((message (format nil "Failed to add relationship ~A from ~A to '~A'. ~A: ~A"
                                        relationship
                                        source
                                        target
                                        status-code
                                        body)))
                   (log-message :debug message)
                   (setf (tbnl:content-type*) "text/html")
                   (setf (tbnl:return-code*) tbnl:+http-bad-request+)
                   (with-output-to-string (outstr)
                     (let ((*standard-output* outstr)
                           (sourceparts (get-uri-parts source tbnl:*acceptor*)))
                       (spinneret:with-html
                         (:doctype)
                         (:html
                           (default-layout :title "Failed to create relationship"
                                           :resourcetype (car (last sourceparts 2))
                                           :uid (car (last sourceparts))
                                           :stylesheets '("display")
                                           :javascripts '("display")
                                           :content (display-default (list :p message))))))))
                 ;; Happy path: no errors
                 (tbnl:redirect (concatenate 'string "/display" source)))))
         ;; We didn't get those parameters
         (progn
           (setf (tbnl:content-type*) "text/html")
           (setf (tbnl:return-code*) tbnl:+http-bad-request+)
           "Need both the <code>source</code> and <code>target</code> parameters. Please try again.")))
    ;; Fallback: not implemented
    (t
      (method-not-allowed))))

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
         ;; Hacky workaround required by Spinneret
         (let ((*standard-output* outstr)
               (title (format nil "Edit tags and links for ~A ~A"
                              resourcetype (uid-to-title resourcename))))
           (spinneret:with-html
             (:doctype)
             (:html
               ;; Dynamically-generated HEAD section
               (spinneret:interpret-html-tree
                 (header :title title
                         :stylesheets '("edit_links")
                         :javascripts '()))
               (:body
                 (:div#pagegrid
                   (:h1 title)
                   (spinneret:interpret-html-tree
                     (navbar-top :resourcetype resourcetype
                                 :uid resourcename))
                   (:form#tagform.formgrid :method "post"
                    :action (format nil "/edit_links~A" resource)
                    (:div#edit-membership
                      (:div#add_tags
                        (:h3 "Add tags")
                        (:select :name "add-tags" :multiple "multiple" :size 10
                         (mapcar #'(lambda (tag)
                                     (:option :value tag tag))
                                 ;; List of tags available to be added,
                                 ;; excluding those already associated
                                 (sort
                                   (set-difference all-tags extant-tags :test #'equal)
                                   #'string<))))
                      (:div#remove_tags
                        (:h3 "Remove tags")
                        (:select :name "remove-tags" :multiple "multiple" :size 10
                         (mapcar #'(lambda (tag)
                                     (:option :value tag tag))
                                 (sort extant-tags #'string<))))
                      (:input#update_membership :type "submit" :value "Update tags")))
                   (:form#relationshipform.formgrid2 :method "get"
                    :action (format nil "/create_relationship")
                    (:div#link-resources
                      (:h3 "Link to other resources")
                      (:p "URI of target resource:")
                      (:input :name "source"
                       :type "hidden"
                       :value (format nil "~{/~A~}" (cdr uri-parts)))
                      (:input#resource_links
                        :name "target"
                        :type "text"
                        :value ""))
                    (:input#link_resource :type "submit" :value "Select relationship"))))))))))
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
               (let ((*standard-output* outstr))
                 (spinneret:with-html
                   (:doctype)
                   (:html
                     (default-layout :title (format nil "Failed to create ~A" uid)
                                     :resourcetype resourcetype
                                     :uid uid
                                     :stylesheets '("display")
                                     :javascripts '("display")
                                     :content (display-default update-errors))))))))
       ;; Happy path: no errors
       (tbnl:redirect (concatenate 'string "/display/" resourcetype "/" uid ))))
    (t (method-not-allowed))))
