sbcl --userinit sbclrc-docker \
    --eval "(asdf:load-system :syscat-gui)" \
    --eval "(sb-ext:save-lisp-and-die \"syscatgui\" :executable t :toplevel #'(lambda () (syscat-gui::dockerstart)))"
