## Introduction

This tool runs as a postgres proxy and monitors all Query, Bind, Exec and Sync messages between frameworks and the postgres database server. It summarises total counts of all these messages in the wire protocol as well as verifying that every Exec command is followed by a paired Sync command.

More details in the github discussion [here](https://github.com/TechEmpower/FrameworkBenchmarks/issues/7381#issuecomment-1146745883).

## Running

To run a verification with the proxy enabled, for a single framework (in this case just-js) and test

```
./tfb --mode verify --test just --proxy on --type db
```

or for a single framework and all tests

```
./tfb --mode verify --test just --proxy on
```

## Automating the Verification and Producing a Report

You can download a verification tool from this [gist](https://gist.github.com/billywhizz/bf02e0f166ba88532c25a79c58d514b1).

Ensure verify.js and index.html are in the root of the Techempower repository, then install just-js runtime as follows:

```
sh -c "$(curl -sSL https://raw.githubusercontent.com/just-js/just/0.1.9/install.sh)"
```

and to run the verification and produce a results.json with all results

```
./just verify.js
```

You can then use a local web server to serve from the root of the repo and, assuming it is serving on port 8080 you can navigate to http://127.0.0.1:8080/index.html to view the results.
