import os
import glob
import json
import copy

from pprint import pprint

class FrameworkTestType:
  '''
  '''
  def __init__(self, name, requires_db = False, args = []):
    self.name = name
    self.requires_db = requires_db
    self.args = args
  
  def parse(self, test_keys):
    '''Takes the dict of key/value pairs describing a FrameworkTest 
    and collects all variables needed by this FrameworkTestType

    Raises AttributeError if required keys are missing
    '''
    if all (arg in test_keys for arg in self.args):
      self.__dict__.update({ arg:test_keys[arg] for arg in self.args})
      return self
    else: # This is quite common - most tests don't support all types
      raise AttributeError("A %s requires the benchmark_config to contain %s"%(self.name,self.args))

  def copy(self):
    '''Returns a copy that can be safely modified. Use before calling 
    parse'''
    return copy.copy(self)

class JsonTestType(FrameworkTestType):
  def __init__(self):
    args = ['json_url']
    FrameworkTestType.__init__(self, 'json', False, args)
  
class DBTestType(FrameworkTestType):
  def __init__(self):
    args = ['db_url']
    FrameworkTestType.__init__(self, 'db', True, args)


class QueryTestType(FrameworkTestType):
  def __init__(self):
    args = ['query_url']
    FrameworkTestType.__init__(self, 'query', True, args)


class FortuneTestType(FrameworkTestType):
  def __init__(self):
    args = ['fortune_url']
    FrameworkTestType.__init__(self, 'fortune', True, args)

class UpdateTestType(FrameworkTestType):
  def __init__(self):
    args = ['update_url']
    FrameworkTestType.__init__(self, 'update', True, args)

class PlaintextTestType(FrameworkTestType):
  def __init__(self):
    args = ['plaintext_url']
    FrameworkTestType.__init__(self, 'plaintext', False, args)
