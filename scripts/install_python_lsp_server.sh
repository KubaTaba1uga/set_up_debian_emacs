#!/bin/bash

EMACS_VIRTUALENV_PATH="/home/taba1uga/.emacs.venv"
EMACS_PYTHON_PATH=$EMACS_VIRTUALENV_PATH"/bin/python"

# install python packages required by language server
sudo apt-get install -y virtualenv

virtualenv $EMACS_VIRTUALENV_PATH

$EMACS_PYTHON_PATH -m pip install python-lsp-server 




