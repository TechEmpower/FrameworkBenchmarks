# Package

version       = "0.1.0"
author        = "Rishav Sharan"
description   = "Techempower Nim-Prologue benchmark"
license       = "MIT"
bin           = @["app"]
skipExt       = @["nim"]

# Dependencies

requires "nim >= 1.4.0"
requires "prologue >= 0.4.0"
