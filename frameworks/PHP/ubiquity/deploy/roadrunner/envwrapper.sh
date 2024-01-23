#!/bin/bash
export NUM_WORKERS=$(($(nproc)*3))
$*