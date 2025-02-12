#!/usr/bin/env bash

EMACS_DIR="${EMACS_DIR:-$HOME/.emacs.d}"

if [ ! -d $EMACS_DIR ]; then
    mkdir -p $EMACS_DIR
fi

if [ -f "$EMACS_DIR/init.el" ]; then
    echo "Existing emacs config detected! Please remove the file '$EMACS_DIR/init.el' to continue."
    exit 1
fi

# Clone emacs-substrate
pushd $EMACS_DIR
git clone https://github.com/Etenil/emacs-substrate.git

# Download and prep early-init.el
sed "s%INSTALL_PATH%$EMACS_DIR/emacs-substrate%g" < emacs-substrate/installer/early-init.el > "$EMACS_DIR/early-init.el"
cp emacs-substrate/installer/init.el init.el

popd

echo "All done! Enjoy Emacs Substrate!"
