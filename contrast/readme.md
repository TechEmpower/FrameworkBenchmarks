# Welcome to the Contrast shim to [TechEmpower Framework Benchmarks (TFB)](http://www.techempower.com/benchmarks/)

This directory provides the shims necessary to run the TFB while instrumenting (some of) the test apps with their respective Contrast Agents.

## Quick Start Guide

There are 6 scripts with filenames in the pattern of `benchmark-agent.sh`. These scripts require the following:

- The agent to test, using the appropriate filename from this list
    - contrast.jar
    - contrast-agent.tar.gz
    - contrast-agent.gem
    - node-contrast.tgz
    - Contrast.NET.Core.zip
    - contrast-go
- Environment Variables
    - CONTRAST__API__URL
    - CONTRAST__API__API_KEY
    - CONTRAST__API__SERVICE_KEY
    - CONTRAST__API__USER_NAME


After that, run the script for the agent you want to use, and wait! When the tests are done running, capture the contents of `../results`.

## FAQ

### The results files are hard to read!
You can use `npm run start $RESULT_DIRECTORY` to create a more human-readable output.