#!/bin/sh

for i in $(seq 0 $(($(nproc --all)-1))); do
	julia server.jl
done

