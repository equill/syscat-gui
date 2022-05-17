;   Copyright 2019-22 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

;;;; Configs for the server to use

(in-package #:syscat-gui)

(defparameter *config-vars*
  `(:listen-address "localhost"
    :listen-port 8083
    :rg-hostname "syscat-docker.onfire.onice"
    :rg-port 4965
    ;:rg-hostname "localhost"
    ;:rg-port 4950
    :api-uri-base "/raw/v1"
    :files-uri-base "/files/v1"
    :schema-uri-base "/schema/v1"
    :static-path "/home/james/devel/restagraph/syscat-gui/src/static/"))

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

;; Enable extra markdown options
(setf 3bmd-code-blocks:*code-blocks* t)
(setf 3bmd-definition-lists:*definition-lists* t)
(setf 3bmd-tables:*tables* t)
;(setf 3bmd-ext-math:*math* t)
