;   Copyright 2019-22 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; The REST API server application

(in-package #:syscat-gui)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


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
           ;(type (or nil syscat-gui-acceptor) acceptor)
           )
  (log-message :info "Attempting to start up the syscat-gui application server")
  ;; Control the decoding of JSON identifiers
  (setf json:*json-identifier-name-to-lisp* 'common-lisp:string-upcase)
  ;; Sanity-check: is an acceptor already running?
  ;;; We can't directly check whether this acceptor is running,
  ;;; so we're using the existence of its special variable as a proxy.
  (if (boundp '*syscat-gui-acceptor*)
    ;; There's an acceptor already in play; bail out.
    (log-message :critical "Acceptor already exists; refusing to create a new one.")
    ;; No existing acceptor; we're good to go.
    (let ((myacceptor (or acceptor (make-acceptor))))
      ;; Make it available as a dynamic variable, for shutdown to work on
      (defparameter *syscat-gui-acceptor* myacceptor)
      ;; Set the dispatch table
      (log-message :info "Configuring the dispatch table")
      (setf tbnl:*dispatch-table*
            (list
              ;; Include the additional dispatchers here
              ;; in alphabetical order, to make it easier to keep track of conflicts
              (tbnl:create-regex-dispatcher "/create$" 'create-item)
              (tbnl:create-prefix-dispatcher "/display" 'display-item)
              (tbnl:create-prefix-dispatcher "/edit_links" 'edit-links)
              (tbnl:create-prefix-dispatcher "/create_relationship" 'select-relationship )
              (tbnl:create-prefix-dispatcher "/editresource" 'edit-resource)
              (tbnl:create-prefix-dispatcher "/files-upload" 'files)
              (tbnl:create-prefix-dispatcher "/image-gallery" 'image-gallery)
              (tbnl:create-prefix-dispatcher "/healthcheck" 'healthcheck)
              (tbnl:create-prefix-dispatcher "/search" 'searchpage)
              (tbnl:create-folder-dispatcher-and-handler "/static/css/"
                                                         (merge-pathnames "css/" (static-path myacceptor))
                                                         "text/css")
              (tbnl:create-folder-dispatcher-and-handler "/static/js/"
                                                         (merge-pathnames "js/" (static-path myacceptor))
                                                         "text/javascript")
              (tbnl:create-prefix-dispatcher "/Tasks" 'tasks)
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

(defun save-image (&optional (path "/tmp/syscat-gui"))
  (sb-ext:save-lisp-and-die path :executable t :toplevel 'syscat-gui::dockerstart))

(defun shutdown ()
  ;; Check whether there's something to shut down
  (if (and
        (boundp '*syscat-gui-acceptor*)
        *syscat-gui-acceptor*)
      ;; There is; go ahead
      (progn
      ;; Check whether it's still present but shutdown
      (if (tbnl::acceptor-shutdown-p *syscat-gui-acceptor*)
          (log-message :info "Acceptor was present but already shut down.")
          (progn
            (log-message :info "Shutting down the syscat-gui application server")
            (handler-case
              ;; Perform a soft shutdown: finish serving any requests in flight
              (tbnl:stop *syscat-gui-acceptor* :soft t)
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
        (makunbound '*syscat-gui-acceptor*))
      ;; No acceptor. Note the fact and do nothing.
      (log-message :warn "No acceptor present; nothing to shut down.")))
