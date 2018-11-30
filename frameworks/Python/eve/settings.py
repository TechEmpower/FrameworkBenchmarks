# -*- coding: utf-8 -*-

import os

RESOURCE_METHODS = ['GET']

ITEM_METHODS = ['GET', 'PATCH', 'DELETE']

CACHE_CONTROL = 'max-age=20'
CACHE_EXPIRES = 20

DOMAIN = {}

RENDERERS = {
    'eve.render.JSONRenderer'
}

DEBUG = True