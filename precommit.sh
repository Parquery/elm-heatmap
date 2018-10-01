#!/bin/bash

SCRIPT_DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
red=`tput setaf 1`

if [ ! $(which elm-format) ]; then
    echo "${red} elm-format (https://github.com/avh4/elm-format) not found."
    exit 1
else
    elm-format --yes src/
    elm-format --yes examples/
    elm-format --yes tests/
fi

if [ ! $(which elm-analyse) ]; then
    echo "${red} elm-analyse (https://stil4m.github.io/elm-analyse/) not found."
    exit 1
else
    elm-analyse src/
    elm-analyse examples/
    elm-analyse tests/
fi

if [ ! $(which elm-doc-test) ]; then
    echo "${red} elm-doc-test (https://www.npmjs.com/package/elm-doc-test) not found."
    exit 1
else
    elm-doc-test
fi

if [ ! $(which elm-test) ]; then
    echo "${red} elm-test (https://www.npmjs.com/package/elm-test) not found."
    exit 1
else
    elm-test
fi