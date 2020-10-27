sbcl --userinit sbclrc-docker \
    --eval "(asdf:load-system :cl-webcat)" \
    --eval "(sb-ext:save-lisp-and-die \"clwebcat\" :executable t :toplevel #'(lambda () (cl-webcat::dockerstart)))"
