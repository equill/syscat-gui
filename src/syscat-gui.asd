;   Copyright 2017-22 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

(asdf:defsystem #:syscat-gui
  :serial t
  :license "GPLv3"
  :author "James Fleming <james@electronic-quill.net>"
  :description "Rudimentary GUI for Syscat"
  :depends-on (#:3bmd
               #:3bmd-ext-code-blocks
               #:3bmd-ext-definition-lists
               #:3bmd-ext-tables
               ;#:3bmd-ext-math
               #:cl-json
               #:cl-ppcre
               #:cl-who
               #:drakma
               #:flexi-streams
               #:hunchentoot)
  :components ((:file "package")
               (:file "config")
               (:file "structures")
               (:file "logging")
               (:file "utilities")
               (:file "errors")
               (:file "dispatchers")
               (:file "dispatchers-cl-who")
               (:file "hunchentoot")))
