;   Copyright 2019 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

;;;; Configs for the server to use

(in-package #:cl-webcat)

(defparameter *config-vars*
  `(:listen-address "localhost"
    :listen-port 8080
    :rg-hostname "localhost"
    :rg-port 4955
    :api-uri-base "/raw/v1"
    :schema-uri-base "/schema/v1"
    :template-path "/home/james/devel/syscat/cl-webcat/src/templates"
    :static-path "/home/james/devel/syscat/cl-webcat/src/static"))

(setf *loglevel* :info)
