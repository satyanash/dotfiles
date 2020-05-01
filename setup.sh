#!/bin/bash

cat <<EOF > "${HOME}/.emacs"
(add-to-list 'load-path "$(pwd)/emacs")
(require 'main)
EOF

ln -s "$(realpath '.bashrc')" "${HOME}/.bashrc"
