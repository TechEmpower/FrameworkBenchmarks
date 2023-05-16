#!/bin/sh
 
julia --threads auto server.jl  
while : ; do sleep 1 ; done
