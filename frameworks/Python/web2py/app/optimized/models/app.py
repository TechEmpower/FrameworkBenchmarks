# -*- coding: utf-8 -*-
from gluon import current
# Re-use the controller from the "standard" application.
from applications.standard.modules import controller

current.optimized = True # Flag used in controller.py to trigger DAL optimizations.
current.response._view_environment = current.globalenv
# To avoid the overhead of executing a controller, the response is returned here.
raise HTTP(200, getattr(controller, request.function)(), **current.response.headers)
