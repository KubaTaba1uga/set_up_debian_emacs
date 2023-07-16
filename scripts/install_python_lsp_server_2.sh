#!/bin/bash

EMACS_VIRTUALENV_PATH="/home/taba1uga/.emacs.venv"
EMACS_PYTHON_PATH=$EMACS_VIRTUALENV_PATH"/bin/python"

# install python packages required by language server
sudo apt-get install -y virtualenv

virtualenv $EMACS_VIRTUALENV_PATH

# install lsp server and extensions
$EMACS_PYTHON_PATH -m pip install python-lsp-server==1.7.3 python-lsp-black python-lsp-ruff pylsp-mypy python-lsp-isort

# install python packages
$EMACS_PYTHON_PATH -m pip install black ruff mypy isort flake8

cp scripts/.emacs ~/.emacs




