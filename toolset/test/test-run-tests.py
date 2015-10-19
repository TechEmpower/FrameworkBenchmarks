#!/usr/bin/env python

import argparse
import ConfigParser
import sys
import os
import multiprocessing
import itertools
import copy
import subprocess
from pprint import pprint 
from benchmark.benchmarker import Benchmarker
from setup.linux.unbuffered import Unbuffered
from setup.linux import setup_util

from run-tests import StoreSeqAction

parser = argparse.ArgumentParser()
parser.add_argument('--foo', action=StoreSeqAction)
tests = ["1", "1,", "0.23",                       # Single numbers
        "1,5,7", "1,2,-3", "1,1000,12,1,1,1,1,1", # Lists
        "1:2:10", "1:2", "10:-2:0",               # Sequences
        "1,2:1:5"                                 # Complex
]
for test in tests:
  try:
    t = "--foo %s" % test
    print "Testing %s" % test
    print "  %s" % parser.parse_args(t.split())
    print "  Success"
  except Exception as e: 
    print "  Exception! %s" % e
    continue

# vim: sw=2
