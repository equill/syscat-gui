;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

(asdf:defsystem #:cl-webcat
  :serial t
  :license "GPLv3"
  :author "James Fleming <james@electronic-quill.net>"
  :description "Generates a REST API from a shema defined in Neo4J"
  :depends-on (#:hunchentoot
               #:drakma
               #:flexi-streams
               #:cl-json
               #:cl-ppcre
               #:html-template
               #:3bmd)
  :components ((:file "package")
               (:file "config")
               (:file "logging")
               (:file "hunchentoot")))
