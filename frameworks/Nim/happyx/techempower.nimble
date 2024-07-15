# Package

version       = "0.1.0"
author        = "Ethosa"
description   = "TechEmpower HappyX benchmark."
license       = "MIT"

bin           = @["techempower"]
skipExt = @["nim"]

# Dependencies

requires "nim >= 1.0.0"

# We lock dependencies here on purpose.
requires "happyx#head"
requires "norm"
