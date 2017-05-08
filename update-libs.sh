#!/bin/bash

# Manaul Updates

## Refresh elpa/
rm -rf elpa

## Prettier
wget https://raw.githubusercontent.com/prettier/prettier/master/editors/emacs/prettier-js.el -O lisp/prettier-js/prettier-js.el


# Submodules
git submodule update --recursive --remote
