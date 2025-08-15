# Introduction

[OpenRun](https://github.com/openrundev/openrun) is a platform for developing and deploying internal tools.

OpenRun is implemented in Go. OpenRun apps are written in [Starlark](https://starlark-lang.org/). Starlark is a thread-safe language with Python syntax, designed for embedding. OpenRun uses the [Starlark Go](https://github.com/google/starlark-go) implementation. Since apps are developed using a python like syntax, the benchmark is added under the Python category.

# Benchmarking

The JSON and plaintext tests are implemented. OpenRun supports SQLite database only currently, so the database tests are not implemented.

The Dockerfile starts the OpenRun server and creates a single app which implements the benchmark apis (app.star).
