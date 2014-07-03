#!/bin/bash
. ../toolset/setup/linux/bash_functions.sh

cabal update
cabal install yesod persistent-mysql