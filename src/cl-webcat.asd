;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

(asdf:defsystem #:cl-webcat
  :serial t
  :license "GPLv3"
  :author "James Fleming <james@electronic-quill.net>"
  :description "Generates a REST API from a shema defined in Neo4J"
  :depends-on (#:3bmd
               #:cl-json
               #:cl-ppcre
               #:drakma
               #:flexi-streams
               #:html-template ; Legacy; to be removed
               #:spinneret    ; HTML-generation library du jour
               #:hunchentoot)
  :components ((:file "package")
               (:file "config")
               (:file "structures")
               (:file "logging")
               (:file "utilities")
               (:file "errors")
               (:file "dispatchers")
               (:file "hunchentoot")))
