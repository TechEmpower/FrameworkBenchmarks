#!/usr/bin/env bash
# aws.sh <bucket-name> <language> <hash> <agent filename>

# Get the agent
aws s3 cp s3://$1/$2/$3/$4 ./

# Run the tests
./benchmark-${2}.sh

# Parse output and push to S3
OUTPUT_DIR=`find ../results -type d -regex "^../results/[0-9]*$"`

npm run start $OUTPUT_DIR

aws s3 cp $OUTPUT_DIR s3://$1/$2/$3/results --recursive
