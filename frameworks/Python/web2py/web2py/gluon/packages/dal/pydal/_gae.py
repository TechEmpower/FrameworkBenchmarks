# -*- coding: utf-8 -*-

try:
    from new import classobj
    from google.appengine.ext import db as gae
    from google.appengine.ext import ndb
    from google.appengine.api import namespace_manager, rdbms
    from google.appengine.api.datastore_types import Key  # for belongs on ID
    from google.appengine.ext.ndb.polymodel import PolyModel as NDBPolyModel
except ImportError:
    gae = None
    Key = None
