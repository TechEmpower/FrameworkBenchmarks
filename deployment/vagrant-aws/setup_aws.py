#!/usr/bin/env python
#
# Prepares Amazon network to run either vagrant-development or
# vagrant-production. Configures subnets, virtual private clouds, 
# security groups, etc
#
# Currently is a one-way operation, you have to delete these 
# manually. Visit https://console.aws.amazon.com/vpc and remove the 
# VPC tagged TFB_Network and that should undo all changes made my 
# this script (for production mode). For development mode, the only 
# things created are a security group and a subnet, so find those in
# your standard EC2 console https://console.aws.amazon.com/ec2 and 
# delete them manually

import subprocess
import json
import logging
import sys

log = logging.getLogger('aws')

nwtags = "Key=Project,Value=FrameworkBenchmarks Key=TFB_Role,Value=network"

def setup_vpc():
  '''Sets up a Virtual Private Cloud to allow hosts to communicate'''
  
  # Setup VPC
  log.info("Creating a new Virtual Private Cloud...")
  log.info(" See details at http://console.aws.amazon.com/vpc")
  vpc = run_aws("create-vpc --cidr-block 172.16.0.0/16 --instance-tenancy default")
  vpcid = vpc["Vpc"]["VpcId"]

  run_aws("modify-vpc-attribute --vpc-id %s --enable-dns-support" % vpcid)
  run_aws("modify-vpc-attribute --vpc-id %s --no-enable-dns-hostnames" % vpcid)
  run_aws("create-tags --resources %s --tags %s Key=Name,Value=TFB_Network" % (vpcid, nwtags))
  log.debug(run_aws("describe-vpcs --vpc-id %s" % vpcid, load=False))

  # Setup internet gateway
  log.info("Creating InternetGateway for the VPC...")
  igw = run_aws("create-internet-gateway")
  igwid = igw["InternetGateway"]["InternetGatewayId"]
  run_aws("create-tags --resources %s --tags %s Key=Name,Value=TFB_Gateway" % (igwid, nwtags))
  run_aws("attach-internet-gateway --internet-gateway-id %s --vpc-id %s" % (igwid, vpcid))
  log.debug(run_aws("describe-internet-gateways --internet-gateway-ids %s" % igwid, load=False))

  # Setup public subnet 
  # NOTE: We considered using a public and private subnet, but 
  # this requires us to launch an extra EC2 instance for the duration of the 
  # benchmark to handle the NAT between the public subnet and the private subnet,
  # so the cost is quite high. Also, Internet traffic is only generated during 
  # framework setup stages (e.g. during software installation), not during the 
  # running of the benchmark. 
  # We chose to use a single public subnet and filter inbound traffic to prevent 
  # interference during the test
  log.info("Creating subnet inside the VPC...")
  pubsub = run_aws("create-subnet --vpc-id %s --cidr-block 172.16.0.0/24" % vpcid)
  pubid = pubsub["Subnet"]["SubnetId"]
  log.debug("Found subnet id: %s", pubid)

  #run_aws("modify-subnet-attribute --subnet-id %s --map-public-ip-on-launch" % pubid)
  run_aws("create-tags --resources %s --tags %s Key=Name,Value=TFB_Public" % (pubid, nwtags))
  log.debug(run_aws("describe-subnets --subnet-ids %s" % pubid, load=False))
  
  # Setup routing
  log.info("Creating routing table for VPC...")
  route = run_aws("describe-route-tables --filters Name=vpc-id,Values=%s" % vpcid)
  routeid = route["RouteTables"][0]["RouteTableId"]
  run_aws("create-tags --resources %s --tags %s Key=Name,Value=TFB_Routing" % (routeid, nwtags))
  log.info("  Creating route to internet...")
  run_aws("create-route --route-table-id %s --destination-cidr-block 0.0.0.0/0 --gateway-id %s" % (routeid, igwid)) 
  log.info("  Associating routing table and subnet...")
  run_aws("associate-route-table --route-table-id %s --subnet-id %s" % (routeid, pubid))

  # Setup default security group for instances launched in the VPC
  log.info("Creating default security group for VPC")
  group = run_aws("create-security-group --group-name TFB_Security --vpc-id %s --description 'FrameworkBenchmarks security group'" % vpcid)
  groupid = group["GroupId"]
  run_aws("create-tags --resources %s --tags %s Key=Name,Value=TFB_Security" % (groupid, nwtags))
  run_aws("authorize-security-group-ingress --group-id %s --protocol tcp --port 22 --cidr 0.0.0.0/0" % groupid)
  # run_aws("authorize-security-group-egress --group-id %s --protocol -1 --cidr 0.0.0.0/0 --port all" % groupid)
  run_aws("authorize-security-group-ingress --group-id %s --source-group %s --protocol -1 --port -1" % (groupid, groupid))

  log.info("Complete."
  log.info(" Here are the environment variables you should use:")
  print "export TFB_AWS_SUBNET=%s" % pubid
  print "export TFB_AWS_SEC_GROUP=%s" % groupid

  return vpcid

def unset_vpc(vpcid):
  '''Doesn't work at the moment, we need to delete all the other items first'''
  run_aws("delete-vpc --vpc-id %s" % vpcid)

def run_aws(command, prefix=True, load=True):
  '''Runs an AWS command and returns the JSON
  prefix: Should we prefix "aws ec2 " to your command
  load: Should we auto-load the response JSON into a python object? 
  '''
  if prefix:
    command = "aws ec2 %s" % command
  log.debug("Request : %s", command)
  result = subprocess.check_output(command, shell=True)
  log.debug("Response: %s", result)
  if load:
    return json.loads(result)
  else:
    return result

if __name__ == "__main__":
  args = sys.argv[1:]

  logging.basicConfig(level=logging.INFO)
  usage = '''Usage: setup_aws.py

Prepares Amazon network to run either vagrant-development 
or vagrant-production. Configures subnets, virtual private
clouds, security groups, etc. 

Outputs TFB_AWS_SEC_GROUP and TFB_AWS_SUBNET

Currently is a one-way operation, you have to delete these 
manually. This script expects standard AWS environment 
variables to exist e.g. AWS_ACCESS_KEY_ID, 
AWS_SECRET_ACCESS_KEY, AWS_DEFAULT_REGION
'''

  if len(args) != 0:
    print usage
    sys.exit(1)

  setup_vpc()
