#!/bin/bash

EMACS_VIRTUALENV_PATH="/home/taba1uga/.emacs.venv"
EMACS_PYTHON_PATH=$EMACS_VIRTUALENV_PATH"/bin/python"

# install python packages required by language server
sudo apt-get install -y virtualenv

virtualenv $EMACS_VIRTUALENV_PATH

# install lsp sevrer and extensions
$EMACS_PYTHON_PATH -m pip install python-lsp-server==1.7.4 python-lsp-black python-lsp-ruff pylsp-mypy python-lsp-isort

# install python packages
$EMACS_PYTHON_PATH -m pip install black ruff mypy isort rope

cp scripts/.emacs ~/.emacs



