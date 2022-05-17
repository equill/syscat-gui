;   Copyright 2022 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

(in-package #:syscat-gui)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


;; Request handlers with no affiliation to content-generation frameworks

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
