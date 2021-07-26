#!/usr/bin/env bash
# aws-integration.sh <bucket-name> <language> <hash> <agent filename>

CURRENT_REGION=`curl -s http://169.254.169.254/latest/dynamic/instance-identity/document | jq .region -r`

CONTRAST_API_SECRETS=`aws secretsmanager get-secret-value --secret-id tech-empower-teamserver-api --region $CURRENT_REGION --query SecretString --output text`

export CONTRAST__API__URL=`echo $CONTRAST_API_SECRETS | jq -r .CONTRAST__API__URL`
export CONTRAST__API__API_KEY=`echo $CONTRAST_API_SECRETS | jq -r .CONTRAST__API__API_KEY`
export CONTRAST__API__SERVICE_KEY=`echo $CONTRAST_API_SECRETS | jq -r .CONTRAST__API__SERVICE_KEY`
export CONTRAST__API__USER_NAME=`echo $CONTRAST_API_SECRETS | jq -r .CONTRAST__API__USER_NAME`

# Get the agent
aws s3 cp s3://$1/$2/$3/$4 ./

# Run the tests
./benchmark-${2}.sh

# Parse output and push to S3
OUTPUT_DIR=`find ../results -type d -regex "^../results/[0-9]*$"`

npm run start $OUTPUT_DIR

aws s3 cp $OUTPUT_DIR s3://$1/$2/$3/results --recursive
