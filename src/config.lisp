;   Copyright 2019 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

;;;; Configs for the server to use

(in-package #:cl-webcat)

(defparameter *config-vars*
  `(:listen-address "localhost"
    :listen-port 8080
    :rg-hostname "10.255.0.1"
    :rg-port 4955
    :dbhostname "10.255.0.1"
    :dbport 7677
    :dbname "graph.db"
    :dbusername "neo4j"
    :dbpasswd "wallaby"
    :api-uri-base "/raw/v1"
    :files-uri-base "/files/v1"
    :schema-uri-base "/schema/v1"
    :template-path "/home/james/devel/syscat/cl-webcat/src/templates/"
    :static-path "/home/james/devel/syscat/cl-webcat/src/static/"))

(setf *loglevel* :debug)
