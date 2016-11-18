# -*- coding: utf-8 -*-

# Controller functions would typically be defined in this file but are instead
# imported from a module to enable sharing with the "optimized" application.
import controller
from gluon import current

current.optimized = False # Flag used in controller.py to trigger DAL optimizations.

session.forget(response) # For speed-up when sessions are not needed.

def index():
    # The first URL arg specifies the controller function to be executed
    # from the controller.py module.
    return getattr(controller, request.args(0))()
