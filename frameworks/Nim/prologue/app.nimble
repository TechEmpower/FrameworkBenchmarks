# Package

version       = "0.1.0"
author        = "Rishav Sharan"
description   = "Techempower Nim-Prologue benchmark"
license       = "MIT"
bin           = @["app"]
skipExt       = @["nim"]

# Dependencies

requires "nim >= 1.0.10"
requires "prologue >= 0.4.0"
