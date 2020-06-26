# Package

version       = "0.1.0"
author        = "Dominik Picheta"
description   = "TechEmpower Jester benchmark."
license       = "MIT"

bin           = @["techempower"]
skipExt = @["nim"]

# Dependencies

requires "nim >= 1.0.0"
requires "jester >= 0.4.3"
