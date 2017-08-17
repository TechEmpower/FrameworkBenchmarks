#!/bin/bash
#
# save the current git commit to results/.commit
#
echo "Saving current git commit to results directory..."
GIT_COMMIT=$(git -C $TFB_REPOPARENT/$TFB_REPONAME rev-parse HEAD)
mkdir -p $TFB_REPOPARENT/$TFB_REPONAME/results
echo $GIT_COMMIT > $TFB_REPOPARENT/$TFB_REPONAME/results/commit_id.txt
echo "Using commit: " $GIT_COMMIT

