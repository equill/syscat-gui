;   Copyright 2019 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


(defpackage cl-webcat
  (:use
    #:cl)
  (:export
    ;; Operational functions
    startup
    shutdown
    log-message
    ;; Conditions
    integrity-error
    client-error
    message
    ;; Functions
    sanitise-uid
    ))
