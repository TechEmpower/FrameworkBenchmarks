# Package

version       = "0.1.0"
author        = "Dominik Picheta"
description   = "TechEmpower Jester benchmark."
license       = "MIT"

bin           = @["techempower"]
skipExt = @["nim"]

# Dependencies

requires "nim >= 0.18.0"
requires "jester#97f6cbc76dfd8"
