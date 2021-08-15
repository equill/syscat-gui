;   Copyright 2019-21 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

;;;; Configs for the server to use

(in-package #:cl-webcat)

(defparameter *config-vars*
  `(:listen-address "localhost"
    :listen-port 8080
    :rg-hostname "10.255.0.1"
    :rg-port 4965
    ;:rg-hostname "localhost"
    ;:rg-port 4950
    :dbhostname "10.255.0.1"
    :dbport 7680
    :dbname "graph.db"
    :dbusername "neo4j"
    :dbpasswd "wrong"
    :api-uri-base "/raw/v1"
    :files-uri-base "/files/v1"
    :schema-uri-base "/schema/v1"
    :template-path "/home/james/devel/restagraph/cl-webcat/src/templates/"
    :static-path "/home/james/devel/restagraph/cl-webcat/src/static/"))

;; Declare this here because it needs to already have been compiled before any
;; references are made to it in json.lisp.
(defparameter +json-lisp-escaped-chars+
  '((#\" . #\")
    (#\\ . #\\)
    (#\/ . #\/)
    (#\b . #\Backspace)
    (#\f . #\)
    (#\n . #\Newline)
    (#\r . #\Return)
    (#\t . #\Tab)
    (#\u . (4 . 16)))
  "Mapping between JSON String escape sequences and Lisp chars.")

(setf *loglevel* :debug)
