#!/bin/bash

fw_depends haskell

cabal update
cabal install yesod persistent-mysql