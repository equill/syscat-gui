sbcl --userinit sbclrc-docker \
    --eval "(asdf:load-system :webcat-gui)" \
    --eval "(sb-ext:save-lisp-and-die \"webcatgui\" :executable t :toplevel #'(lambda () (webcat-gui::dockerstart)))"
