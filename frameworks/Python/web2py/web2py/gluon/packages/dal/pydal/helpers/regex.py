# -*- coding: utf-8 -*-

import re

REGEX_TYPE = re.compile('^([\w\_\:]+)')
REGEX_DBNAME = re.compile('^(\w+)(\:\w+)*')
REGEX_W = re.compile('^\w+$')
REGEX_TABLE_DOT_FIELD = re.compile('^(\w+)\.([^.]+)$')
REGEX_NO_GREEDY_ENTITY_NAME = r'(.+?)'
REGEX_UPLOAD_PATTERN = re.compile('(?P<table>[\w\-]+)\.(?P<field>[\w\-]+)\.(?P<uuidkey>[\w\-]+)(\.(?P<name>\w+))?\.\w+$')
REGEX_CLEANUP_FN = re.compile('[\'"\s;]+')
REGEX_UNPACK = re.compile('(?<!\|)\|(?!\|)')
REGEX_PYTHON_KEYWORDS = re.compile('^(and|del|from|not|while|as|elif|global|or|with|assert|else|if|pass|yield|break|except|import|print|class|exec|in|raise|continue|finally|is|return|def|for|lambda|try)$')
REGEX_SELECT_AS_PARSER = re.compile("\s+AS\s+(\S+)")
REGEX_CONST_STRING = re.compile('(\"[^\"]*?\")|(\'[^\']*?\')')
REGEX_SEARCH_PATTERN = re.compile('^{[^\.]+\.[^\.]+(\.(lt|gt|le|ge|eq|ne|contains|startswith|year|month|day|hour|minute|second))?(\.not)?}$')
REGEX_SQUARE_BRACKETS = re.compile('^.+\[.+\]$')
REGEX_STORE_PATTERN = re.compile('\.(?P<e>\w{1,5})$')
REGEX_QUOTES = re.compile("'[^']*'")
REGEX_ALPHANUMERIC = re.compile('^[0-9a-zA-Z]\w*$')
REGEX_PASSWORD = re.compile('\://([^:@]*)\:')
REGEX_NOPASSWD = re.compile('\/\/[\w\.\-]+[\:\/](.+)(?=@)') # was '(?<=[\:\/])([^:@/]+)(?=@.+)'
