#!/bin/bash
#
# save the current git commit to results/.commit
#
echo "Saving current git commit to results directory..."
GIT_COMMIT=$(git rev-parse HEAD)
mkdir -p results
echo $GIT_COMMIT > results/.commit
echo "Using commit: " $GIT_COMMIT

