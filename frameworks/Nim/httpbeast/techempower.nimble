# Package

version       = "0.1.0"
author        = "Dominik Picheta"
description   = "TechEmpower HttpBeast benchmark."
license       = "MIT"

bin           = @["techempower"]
skipExt = @["nim"]

# Dependencies

requires "nim >= 0.18.0"
requires "httpbeast#995fe86443d4a0"
