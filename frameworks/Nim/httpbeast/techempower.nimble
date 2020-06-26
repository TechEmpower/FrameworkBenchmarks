# Package

version       = "0.1.0"
author        = "Dominik Picheta"
description   = "TechEmpower HttpBeast benchmark."
license       = "MIT"

bin           = @["techempower"]
skipExt = @["nim"]

# Dependencies

requires "nim >= 1.0.0"
requires "httpbeast >= 0.2.2"
