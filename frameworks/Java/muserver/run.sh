#!/bin/bash

# PROFILING: -XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints
JAVA_OPTIONS="-XX:+UseNUMA \
  -XX:+UseZGC \
  -XX:+ZGenerational \
  $@"

java $JAVA_OPTIONS -cp app.jar benchmark.TFBRest
