#!/bin/bash

cat <<EOF > "${HOME}/.emacs"
(add-to-list 'load-path "$(pwd)/emacs")
(require 'main)
EOF

ln -s "$(realpath '.bashrc')" "${HOME}/.bashrc"

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    ln -s "$(realpath '.i3status.conf')" "${HOME}/.i3status.conf"
    ln -s "$(realpath '.i3')" "${HOME}/.i3"
fi
