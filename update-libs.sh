#!/bin/bash

# Manaul Updates

## Refresh elpa/
rm -rf elpa

## Prettier
wget https://raw.githubusercontent.com/prettier/prettier-emacs/master/prettier-js.el -O lisp/prettier-emacs/prettier-js.el


# Submodules
git submodule update --recursive --remote
