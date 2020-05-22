#!/bin/bash

cat <<EOF > "${HOME}/.emacs"
(add-to-list 'load-path "$(pwd)/emacs")
(setq custom-file "$(pwd)/emacs/main.el")
(require 'main)
EOF

ln -s "$(realpath '.bashrc')" "${HOME}/.bashrc"
