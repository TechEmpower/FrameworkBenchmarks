#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Read from configuration files easily without hurting performances

USAGE:
During development you can load a config file either in .ini or .json
format (by default app/private/appconfig.ini or app/private/appconfig.json)
The result is a dict holding the configured values. Passing reload=True
is meant only for development: in production, leave reload to False and all
values will be cached

from gluon.contrib.appconfig import AppConfig
myconfig = AppConfig(path_to_configfile, reload=False)

print myconfig['db']['uri']

The returned dict can walk with "dot notation" an arbitrarely nested dict

print myconfig.take('db.uri')

You can even pass a cast function, i.e.

print myconfig.take('auth.expiration', cast=int)

Once the value has been fetched (and casted) it won't change until the process
is restarted (or reload=True is passed).

"""
import thread
import os
from ConfigParser import SafeConfigParser
from gluon import current
from gluon.serializers import json_parser

locker = thread.allocate_lock()


def AppConfig(*args, **vars):

    locker.acquire()
    reload_ = vars.pop('reload', False)
    try:
        instance_name = 'AppConfig_' + current.request.application
        if reload_ or not hasattr(AppConfig, instance_name):
            setattr(AppConfig, instance_name, AppConfigLoader(*args, **vars))
        return getattr(AppConfig, instance_name).settings
    finally:
        locker.release()


class AppConfigDict(dict):
    """
    dict that has a .take() method to fetch nested values and puts
    them into cache
    """

    def __init__(self, *args, **kwargs):
        dict.__init__(self, *args, **kwargs)
        self.int_cache = {}

    def take(self, path, cast=None):
        parts = path.split('.')
        if path in self.int_cache:
            return self.int_cache[path]
        value = self
        walking = []
        for part in parts:
            if part not in value:
                raise BaseException("%s not in config [%s]" %
                    (part, '-->'.join(walking)))
            value = value[part]
            walking.append(part)
        if cast is None:
            self.int_cache[path] = value
        else:
            try:
                value = cast(value)
                self.int_cache[path] = value
            except (ValueError, TypeError):
                raise BaseException("%s can't be converted to %s" %
                 (value, cast))
        return value


class AppConfigLoader(object):

    def __init__(self, configfile=None):
        if not configfile:
            priv_folder = os.path.join(current.request.folder, 'private')
            configfile = os.path.join(priv_folder, 'appconfig.ini')
            if not os.path.isfile(configfile):
                configfile = os.path.join(priv_folder, 'appconfig.json')
                if not os.path.isfile(configfile):
                    configfile = None
        if not configfile or not os.path.isfile(configfile):
            raise BaseException("Config file not found")
        self.file = configfile
        self.ctype = os.path.splitext(configfile)[1][1:]
        self.settings = None
        self.read_config()

    def read_config_ini(self):
        config = SafeConfigParser()
        config.read(self.file)
        settings = {}
        for section in config.sections():
            settings[section] = {}
            for option in config.options(section):
                settings[section][option] = config.get(section, option)
        self.settings = AppConfigDict(settings)

    def read_config_json(self):
        with open(self.file, 'r') as c:
            self.settings = AppConfigDict(json_parser.load(c))

    def read_config(self):
        if self.settings is None:
            try:
                getattr(self, 'read_config_' + self.ctype)()
            except AttributeError:
                raise BaseException("Unsupported config file format")
        return self.settings
