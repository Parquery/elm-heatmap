#!/bin/bash

SCRIPT_DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ ! $(which elm-format) ]; then
    echo "elm-format (https://github.com/avh4/elm-format) not found."
else
    elm-format --yes src/
    elm-format --yes examples/
fi

if [ ! $(which elm-analyse) ]; then
    echo "elm-analyse (https://stil4m.github.io/elm-analyse/) not found."
else
    elm-analyse src/
    elm-analyse examples/
fi