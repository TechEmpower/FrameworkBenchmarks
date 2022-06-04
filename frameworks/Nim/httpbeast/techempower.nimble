# Package

version       = "0.1.0"
author        = "Dominik Picheta"
description   = "TechEmpower HttpBeast benchmark."
license       = "MIT"

bin           = @["techempower"]
skipExt = @["nim"]

# Dependencies

# We lock dependencies here on purpose.
requires "httpbeast#v0.4.0"
