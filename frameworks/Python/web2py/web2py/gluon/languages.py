#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
| This file is part of the web2py Web Framework
| Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
| License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)
| Plural subsystem is created by Vladyslav Kozlovskyy (Ukraine) <dbdevelop@gmail.com>

Translation system
--------------------------------------------
"""

import os
import re
import sys
import pkgutil
import logging
from cgi import escape
from threading import RLock

try:
    import copyreg as copy_reg # python 3
except ImportError:
    import copy_reg # python 2

from gluon.portalocker import read_locked, LockedFile
from utf8 import Utf8

from gluon.fileutils import listdir
from gluon.cfs import getcfs
from gluon.html import XML, xmlescape
from gluon.contrib.markmin.markmin2html import render, markmin_escape
from string import maketrans

__all__ = ['translator', 'findT', 'update_all_languages']

ostat = os.stat
oslistdir = os.listdir
pjoin = os.path.join
pexists = os.path.exists
pdirname = os.path.dirname
isdir = os.path.isdir

DEFAULT_LANGUAGE = 'en'
DEFAULT_LANGUAGE_NAME = 'English'

# DEFAULT PLURAL-FORMS RULES:
# language doesn't use plural forms
DEFAULT_NPLURALS = 1
# only one singular/plural form is used
DEFAULT_GET_PLURAL_ID = lambda n: 0
# word is unchangeable
DEFAULT_CONSTRUCT_PLURAL_FORM = lambda word, plural_id: word

NUMBERS = (int, long, float)

# pattern to find T(blah blah blah) expressions
PY_STRING_LITERAL_RE = r'(?<=[^\w]T\()(?P<name>'\
    + r"[uU]?[rR]?(?:'''(?:[^']|'{1,2}(?!'))*''')|"\
    + r"(?:'(?:[^'\\]|\\.)*')|" + r'(?:"""(?:[^"]|"{1,2}(?!"))*""")|'\
    + r'(?:"(?:[^"\\]|\\.)*"))'

regex_translate = re.compile(PY_STRING_LITERAL_RE, re.DOTALL)
regex_param = re.compile(r'{(?P<s>.+?)}')

# pattern for a valid accept_language
regex_language = \
    re.compile('([a-z]{2,3}(?:\-[a-z]{2})?(?:\-[a-z]{2})?)(?:[,;]|$)')
regex_langfile = re.compile('^[a-z]{2,3}(-[a-z]{2})?\.py$')
regex_backslash = re.compile(r"\\([\\{}%])")
regex_plural = re.compile('%({.+?})')
regex_plural_dict = re.compile('^{(?P<w>[^()[\]][^()[\]]*?)\((?P<n>[^()\[\]]+)\)}$')  # %%{word(varname or number)}
regex_plural_tuple = re.compile(
    '^{(?P<w>[^[\]()]+)(?:\[(?P<i>\d+)\])?}$')  # %%{word[index]} or %%{word}
regex_plural_file = re.compile('^plural-[a-zA-Z]{2}(-[a-zA-Z]{2})?\.py$')


def is_writable():
    """ returns True if and only if the filesystem is writable """
    from gluon.settings import global_settings
    return not global_settings.web2py_runtime_gae


def safe_eval(text):
    if text.strip():
        try:
            import ast
            return ast.literal_eval(text)
        except ImportError:
            return eval(text, {}, {})
    return None

# used as default filter in translator.M()


def markmin(s):
    def markmin_aux(m):
        return '{%s}' % markmin_escape(m.group('s'))
    return render(regex_param.sub(markmin_aux, s),
                  sep='br', autolinks=None, id_prefix='')

# UTF8 helper functions


def upper_fun(s):
    return unicode(s, 'utf-8').upper().encode('utf-8')


def title_fun(s):
    return unicode(s, 'utf-8').title().encode('utf-8')


def cap_fun(s):
    return unicode(s, 'utf-8').capitalize().encode('utf-8')
ttab_in = maketrans("\\%{}", '\x1c\x1d\x1e\x1f')
ttab_out = maketrans('\x1c\x1d\x1e\x1f', "\\%{}")

# cache of translated messages:
# global_language_cache:
# { 'languages/xx.py':
#     ( {"def-message": "xx-message",
#        ...
#        "def-message": "xx-message"}, lock_object )
#  'languages/yy.py': ( {dict}, lock_object )
#  ...
# }

global_language_cache = {}


def get_from_cache(cache, val, fun):
    lang_dict, lock = cache
    lock.acquire()
    try:
        result = lang_dict.get(val)
    finally:
        lock.release()
    if result:
        return result
    lock.acquire()
    try:
        result = lang_dict.setdefault(val, fun())
    finally:
        lock.release()
    return result


def clear_cache(filename):
    cache = global_language_cache.setdefault(
        filename, ({}, RLock()))
    lang_dict, lock = cache
    lock.acquire()
    try:
        lang_dict.clear()
    finally:
        lock.release()


def read_dict_aux(filename):
    lang_text = read_locked(filename).replace('\r\n', '\n')
    clear_cache(filename)
    try:
        return safe_eval(lang_text) or {}
    except Exception:
        e = sys.exc_info()[1]
        status = 'Syntax error in %s (%s)' % (filename, e)
        logging.error(status)
        return {'__corrupted__': status}


def read_dict(filename):
    """ Returns dictionary with translation messages
    """
    return getcfs('lang:' + filename, filename,
                  lambda: read_dict_aux(filename))


def read_possible_plural_rules():
    """
    Creates list of all possible plural rules files
    The result is cached in PLURAL_RULES dictionary to increase speed
    """
    plurals = {}
    try:
        import gluon.contrib.plural_rules as package
        for importer, modname, ispkg in pkgutil.iter_modules(package.__path__):
            if len(modname) == 2:
                module = __import__(package.__name__ + '.' + modname,
                                    fromlist=[modname])
                lang = modname
                pname = modname + '.py'
                nplurals = getattr(module, 'nplurals', DEFAULT_NPLURALS)
                get_plural_id = getattr(
                    module, 'get_plural_id',
                    DEFAULT_GET_PLURAL_ID)
                construct_plural_form = getattr(
                    module, 'construct_plural_form',
                    DEFAULT_CONSTRUCT_PLURAL_FORM)
                plurals[lang] = (lang, nplurals, get_plural_id,
                                 construct_plural_form)
    except ImportError:
        e = sys.exc_info()[1]
        logging.warn('Unable to import plural rules: %s' % e)
    return plurals

PLURAL_RULES = read_possible_plural_rules()


def read_possible_languages_aux(langdir):
    def get_lang_struct(lang, langcode, langname, langfile_mtime):
        if lang == 'default':
            real_lang = langcode.lower()
        else:
            real_lang = lang
        (prules_langcode,
         nplurals,
         get_plural_id,
         construct_plural_form
         ) = PLURAL_RULES.get(real_lang[:2], ('default',
                                              DEFAULT_NPLURALS,
                                              DEFAULT_GET_PLURAL_ID,
                                              DEFAULT_CONSTRUCT_PLURAL_FORM))
        if prules_langcode != 'default':
            (pluraldict_fname,
             pluraldict_mtime) = plurals.get(real_lang,
                                             plurals.get(real_lang[:2],
                                                         ('plural-%s.py' % real_lang, 0)))
        else:
            pluraldict_fname = None
            pluraldict_mtime = 0
        return (langcode,        # language code from !langcode!
                langname,
                # language name in national spelling from !langname!
                langfile_mtime,  # m_time of language file
                pluraldict_fname,  # name of plural dictionary file or None (when default.py is not exist)
                pluraldict_mtime,  # m_time of plural dictionary file or 0 if file is not exist
                prules_langcode,  # code of plural rules language or 'default'
                nplurals,        # nplurals for current language
                get_plural_id,   # get_plural_id() for current language
                construct_plural_form)  # construct_plural_form() for current language

    plurals = {}
    flist = oslistdir(langdir) if isdir(langdir) else []

    # scan languages directory for plural dict files:
    for pname in flist:
        if regex_plural_file.match(pname):
            plurals[pname[7:-3]] = (pname,
                                    ostat(pjoin(langdir, pname)).st_mtime)
    langs = {}
    # scan languages directory for langfiles:
    for fname in flist:
        if regex_langfile.match(fname) or fname == 'default.py':
            fname_with_path = pjoin(langdir, fname)
            d = read_dict(fname_with_path)
            lang = fname[:-3]
            langcode = d.get('!langcode!', lang if lang != 'default'
                             else DEFAULT_LANGUAGE)
            langname = d.get('!langname!', langcode)
            langfile_mtime = ostat(fname_with_path).st_mtime
            langs[lang] = get_lang_struct(lang, langcode,
                                          langname, langfile_mtime)
    if 'default' not in langs:
        # if default.py is not found,
        # add DEFAULT_LANGUAGE as default language:
        langs['default'] = get_lang_struct('default', DEFAULT_LANGUAGE,
                                           DEFAULT_LANGUAGE_NAME, 0)
    deflang = langs['default']
    deflangcode = deflang[0]
    if deflangcode not in langs:
        # create language from default.py:
        langs[deflangcode] = deflang[:2] + (0,) + deflang[3:]

    return langs


def read_possible_languages(langpath):
    return getcfs('langs:' + langpath, langpath,
                  lambda: read_possible_languages_aux(langpath))


def read_plural_dict_aux(filename):
    lang_text = read_locked(filename).replace('\r\n', '\n')
    try:
        return eval(lang_text) or {}
    except Exception:
        e = sys.exc_info()[1]
        status = 'Syntax error in %s (%s)' % (filename, e)
        logging.error(status)
        return {'__corrupted__': status}


def read_plural_dict(filename):
    return getcfs('plurals:' + filename, filename,
                  lambda: read_plural_dict_aux(filename))


def write_plural_dict(filename, contents):
    if '__corrupted__' in contents:
        return
    fp = None
    try:
        fp = LockedFile(filename, 'w')
        fp.write('#!/usr/bin/env python\n# -*- coding: utf-8 -*-\n{\n# "singular form (0)": ["first plural form (1)", "second plural form (2)", ...],\n')
        for key in sorted(contents, sort_function):
            forms = '[' + ','.join([repr(Utf8(form))
                                    for form in contents[key]]) + ']'
            fp.write('%s: %s,\n' % (repr(Utf8(key)), forms))
        fp.write('}\n')
    except (IOError, OSError):
        if is_writable():
            logging.warning('Unable to write to file %s' % filename)
        return
    finally:
        if fp:
            fp.close()


def sort_function(x, y):
    return cmp(unicode(x, 'utf-8').lower(), unicode(y, 'utf-8').lower())


def write_dict(filename, contents):
    if '__corrupted__' in contents:
        return
    fp = None
    try:
        fp = LockedFile(filename, 'w')
        fp.write('# -*- coding: utf-8 -*-\n{\n')
        for key in sorted(contents, sort_function):
            fp.write('%s: %s,\n' % (repr(Utf8(key)),
                                    repr(Utf8(contents[key]))))
        fp.write('}\n')
    except (IOError, OSError):
        if is_writable():
            logging.warning('Unable to write to file %s' % filename)
        return
    finally:
        if fp:
            fp.close()


class lazyT(object):
    """
    Never to be called explicitly, returned by
    translator.__call__() or translator.M()
    """
    m = s = T = f = t = None
    M = is_copy = False

    def __init__(
        self,
        message,
        symbols={},
        T=None,
        filter=None,
        ftag=None,
        M=False
    ):
        if isinstance(message, lazyT):
            self.m = message.m
            self.s = message.s
            self.T = message.T
            self.f = message.f
            self.t = message.t
            self.M = message.M
            self.is_copy = True
        else:
            self.m = message
            self.s = symbols
            self.T = T
            self.f = filter
            self.t = ftag
            self.M = M
            self.is_copy = False

    def __repr__(self):
        return "<lazyT %s>" % (repr(Utf8(self.m)), )

    def __str__(self):
        return str(self.T.apply_filter(self.m, self.s, self.f, self.t) if self.M else
                   self.T.translate(self.m, self.s))

    def __eq__(self, other):
        return str(self) == str(other)

    def __ne__(self, other):
        return str(self) != str(other)

    def __add__(self, other):
        return '%s%s' % (self, other)

    def __radd__(self, other):
        return '%s%s' % (other, self)

    def __mul__(self, other):
        return str(self) * other

    def __cmp__(self, other):
        return cmp(str(self), str(other))

    def __hash__(self):
        return hash(str(self))

    def __getattr__(self, name):
        return getattr(str(self), name)

    def __getitem__(self, i):
        return str(self)[i]

    def __getslice__(self, i, j):
        return str(self)[i:j]

    def __iter__(self):
        for c in str(self):
            yield c

    def __len__(self):
        return len(str(self))

    def xml(self):
        return str(self) if self.M else escape(str(self))

    def encode(self, *a, **b):
        return str(self).encode(*a, **b)

    def decode(self, *a, **b):
        return str(self).decode(*a, **b)

    def read(self):
        return str(self)

    def __mod__(self, symbols):
        if self.is_copy:
            return lazyT(self)
        return lazyT(self.m, symbols, self.T, self.f, self.t, self.M)


def pickle_lazyT(c):
    return str, (c.xml(),)

copy_reg.pickle(lazyT, pickle_lazyT)


class translator(object):
    """
    This class is instantiated by gluon.compileapp.build_environment
    as the T object

    Example:

        T.force(None) # turns off translation
        T.force('fr, it') # forces web2py to translate using fr.py or it.py

        T("Hello World") # translates "Hello World" using the selected file

    Note:
        - there is no need to force since, by default, T uses
          http_accept_language to determine a translation file.
        - en and en-en are considered different languages!
        - if language xx-yy is not found force() probes other similar languages
          using such algorithm: `xx-yy.py -> xx.py -> xx-yy*.py -> xx*.py`
    """

    def __init__(self, langpath, http_accept_language):
        self.langpath = langpath
        self.http_accept_language = http_accept_language
        # filled in self.force():
        # ------------------------
        # self.cache
        # self.accepted_language
        # self.language_file
        # self.plural_language
        # self.nplurals
        # self.get_plural_id
        # self.construct_plural_form
        # self.plural_file
        # self.plural_dict
        # self.requested_languages
        # ----------------------------------------
        # filled in self.set_current_languages():
        # ----------------------------------------
        # self.default_language_file
        # self.default_t
        # self.current_languages
        self.set_current_languages()
        self.lazy = True
        self.otherTs = {}
        self.filter = markmin
        self.ftag = 'markmin'
        self.ns = None
        self.is_writable = True

    def get_possible_languages_info(self, lang=None):
        """
        Returns info for selected language or dictionary with all
        possible languages info from `APP/languages/*.py`
        It Returns:

        - a tuple containing::

                langcode, langname, langfile_mtime,
                pluraldict_fname, pluraldict_mtime,
                prules_langcode, nplurals,
                get_plural_id, construct_plural_form

                or None

        - if *lang* is NOT defined a dictionary with all possible
          languages::

            { langcode(from filename):
                ( langcode,        # language code from !langcode!
                  langname,
                      # language name in national spelling from !langname!
                  langfile_mtime,  # m_time of language file
                  pluraldict_fname,# name of plural dictionary file or None (when default.py is not exist)
                  pluraldict_mtime,# m_time of plural dictionary file or 0 if file is not exist
                  prules_langcode, # code of plural rules language or 'default'
                  nplurals,        # nplurals for current language
                  get_plural_id,   # get_plural_id() for current language
                  construct_plural_form) # construct_plural_form() for current language
            }

        Args:
            lang (str): language

        """
        info = read_possible_languages(self.langpath)
        if lang:
            info = info.get(lang)
        return info

    def get_possible_languages(self):
        """ Gets list of all possible languages for current application """
        return list(set(self.current_languages +
                        [lang for lang in read_possible_languages(self.langpath).iterkeys()
                         if lang != 'default']))

    def set_current_languages(self, *languages):
        """
        Sets current AKA "default" languages
        Setting one of this languages makes the force() function to turn
        translation off
        """
        if len(languages) == 1 and isinstance(languages[0], (tuple, list)):
            languages = languages[0]
        if not languages or languages[0] is None:
            # set default language from default.py/DEFAULT_LANGUAGE
            pl_info = self.get_possible_languages_info('default')
            if pl_info[2] == 0:  # langfile_mtime
                # if languages/default.py is not found
                self.default_language_file = self.langpath
                self.default_t = {}
                self.current_languages = [DEFAULT_LANGUAGE]
            else:
                self.default_language_file = pjoin(self.langpath,
                                                   'default.py')
                self.default_t = read_dict(self.default_language_file)
                self.current_languages = [pl_info[0]]  # !langcode!
        else:
            self.current_languages = list(languages)
        self.force(self.http_accept_language)

    def plural(self, word, n):
        """
        Gets plural form of word for number *n*
        invoked from T()/T.M() in `%%{}` tag

        Note:
            "word" MUST be defined in current language (T.accepted_language)

        Args:
            word (str): word in singular
            n (numeric): number plural form created for

        Returns:
            word (str): word in appropriate singular/plural form

        """
        if int(n) == 1:
            return word
        elif word:
            id = self.get_plural_id(abs(int(n)))
            # id = 0 singular form
            # id = 1 first plural form
            # id = 2 second plural form
            # etc.
            if id != 0:
                forms = self.plural_dict.get(word, [])
                if len(forms) >= id:
                    # have this plural form:
                    return forms[id - 1]
                else:
                    # guessing this plural form
                    forms += [''] * (self.nplurals - len(forms) - 1)
                    form = self.construct_plural_form(word, id)
                    forms[id - 1] = form
                    self.plural_dict[word] = forms
                    if self.is_writable and is_writable() and self.plural_file:
                        write_plural_dict(self.plural_file,
                                          self.plural_dict)
                    return form
        return word

    def force(self, *languages):
        """
        Selects language(s) for translation

        if a list of languages is passed as a parameter,
        the first language from this list that matches the ones
        from the possible_languages dictionary will be
        selected

        default language will be selected if none
        of them matches possible_languages.
        """
        pl_info = read_possible_languages(self.langpath)

        def set_plural(language):
            """
            initialize plural forms subsystem
            """
            lang_info = pl_info.get(language)
            if lang_info:
                (pname,
                 pmtime,
                 self.plural_language,
                 self.nplurals,
                 self.get_plural_id,
                 self.construct_plural_form
                 ) = lang_info[3:]
                pdict = {}
                if pname:
                    pname = pjoin(self.langpath, pname)
                    if pmtime != 0:
                        pdict = read_plural_dict(pname)
                self.plural_file = pname
                self.plural_dict = pdict
            else:
                self.plural_language = 'default'
                self.nplurals = DEFAULT_NPLURALS
                self.get_plural_id = DEFAULT_GET_PLURAL_ID
                self.construct_plural_form = DEFAULT_CONSTRUCT_PLURAL_FORM
                self.plural_file = None
                self.plural_dict = {}
        language = ''
        if len(languages) == 1 and isinstance(languages[0], str):
            languages = regex_language.findall(languages[0].lower())
        elif not languages or languages[0] is None:
            languages = []
        self.requested_languages = languages = tuple(languages)
        if languages:
            all_languages = set(lang for lang in pl_info.iterkeys()
                                if lang != 'default') \
                | set(self.current_languages)
            for lang in languages:
                # compare "aa-bb" | "aa" from *language* parameter
                # with strings from langlist using such alghorythm:
                # xx-yy.py -> xx.py -> xx*.py
                lang5 = lang[:5]
                if lang5 in all_languages:
                    language = lang5
                else:
                    lang2 = lang[:2]
                    if len(lang5) > 2 and lang2 in all_languages:
                        language = lang2
                    else:
                        for l in all_languages:
                            if l[:2] == lang2:
                                language = l
                if language:
                    if language in self.current_languages:
                        break
                    self.language_file = pjoin(self.langpath, language + '.py')
                    self.t = read_dict(self.language_file)
                    self.cache = global_language_cache.setdefault(
                        self.language_file,
                        ({}, RLock()))
                    set_plural(language)
                    self.accepted_language = language
                    return languages
        self.accepted_language = language
        if not language:
            if self.current_languages:
                self.accepted_language = self.current_languages[0]
            else:
                self.accepted_language = DEFAULT_LANGUAGE
        self.language_file = self.default_language_file
        self.cache = global_language_cache.setdefault(self.language_file,
                                                      ({}, RLock()))
        self.t = self.default_t
        set_plural(self.accepted_language)
        return languages

    def __call__(self, message, symbols={}, language=None, lazy=None, ns=None):
        """
        get cached translated plain text message with inserted parameters(symbols)
        if lazy==True lazyT object is returned
        """
        if lazy is None:
            lazy = self.lazy
        if not language and not ns:
            if lazy:
                return lazyT(message, symbols, self)
            else:
                return self.translate(message, symbols)
        else:
            if ns:
                if ns != self.ns:
                    self.langpath = os.path.join(self.langpath, ns)
                if self.ns is None:
                    self.ns = ns
            otherT = self.__get_otherT__(language, ns)
            return otherT(message, symbols, lazy=lazy)

    def __get_otherT__(self, language=None, namespace=None):
        if not language and not namespace:
            raise Exception('Incorrect parameters')

        if namespace:
            if language:
                index = '%s/%s' % (namespace, language)
            else:
                index = namespace
        else:
            index = language
        try:
            otherT = self.otherTs[index]
        except KeyError:
            otherT = self.otherTs[index] = translator(self.langpath,
                                                      self.http_accept_language)
            if language:
                otherT.force(language)
        return otherT

    def apply_filter(self, message, symbols={}, filter=None, ftag=None):
        def get_tr(message, prefix, filter):
            s = self.get_t(message, prefix)
            return filter(s) if filter else self.filter(s)
        if filter:
            prefix = '@' + (ftag or 'userdef') + '\x01'
        else:
            prefix = '@' + self.ftag + '\x01'
        message = get_from_cache(
            self.cache, prefix + message,
            lambda: get_tr(message, prefix, filter))
        if symbols or symbols == 0 or symbols == "":
            if isinstance(symbols, dict):
                symbols.update(
                    (key, xmlescape(value).translate(ttab_in))
                    for key, value in symbols.iteritems()
                    if not isinstance(value, NUMBERS))
            else:
                if not isinstance(symbols, tuple):
                    symbols = (symbols,)
                symbols = tuple(
                    value if isinstance(value, NUMBERS)
                    else xmlescape(value).translate(ttab_in)
                    for value in symbols)
            message = self.params_substitution(message, symbols)
        return XML(message.translate(ttab_out))

    def M(self, message, symbols={}, language=None,
          lazy=None, filter=None, ftag=None, ns=None):
        """
        Gets cached translated markmin-message with inserted parametes
        if lazy==True lazyT object is returned
        """
        if lazy is None:
            lazy = self.lazy
        if not language and not ns:
            if lazy:
                return lazyT(message, symbols, self, filter, ftag, True)
            else:
                return self.apply_filter(message, symbols, filter, ftag)
        else:
            if ns:
                self.langpath = os.path.join(self.langpath, ns)
            otherT = self.__get_otherT__(language, ns)
            return otherT.M(message, symbols, lazy=lazy)

    def get_t(self, message, prefix=''):
        """
        Use ## to add a comment into a translation string
        the comment can be useful do discriminate different possible
        translations for the same string (for example different locations)::

            T(' hello world ') -> ' hello world '
            T(' hello world ## token') -> ' hello world '
            T('hello ## world## token') -> 'hello ## world'

        the ## notation is ignored in multiline strings and strings that
        start with ##. This is needed to allow markmin syntax to be translated
        """
        if isinstance(message, unicode):
            message = message.encode('utf8')
        if isinstance(prefix, unicode):
            prefix = prefix.encode('utf8')
        key = prefix + message
        mt = self.t.get(key, None)
        if mt is not None:
            return mt
        # we did not find a translation
        if message.find('##') > 0 and not '\n' in message:
            # remove comments
            message = message.rsplit('##', 1)[0]
        # guess translation same as original
        self.t[key] = mt = self.default_t.get(key, message)
        # update language file for latter translation
        if self.is_writable and is_writable() and \
                self.language_file != self.default_language_file:
            write_dict(self.language_file, self.t)
        return regex_backslash.sub(
            lambda m: m.group(1).translate(ttab_in), mt)

    def params_substitution(self, message, symbols):
        """
        Substitutes parameters from symbols into message using %.
        also parse `%%{}` placeholders for plural-forms processing.

        Returns:
            string with parameters

        Note:
            *symbols* MUST BE OR tuple OR dict of parameters!
        """
        def sub_plural(m):
            """String in `%{}` is transformed by this rules:
               If string starts with  `\\`, `!` or `?` such transformations
               take place::

                   "!string of words" -> "String of word" (Capitalize)
                   "!!string of words" -> "String Of Word" (Title)
                   "!!!string of words" -> "STRING OF WORD" (Upper)
                   "\\!string of words" -> "!string of word"
                                 (remove \\ and disable transformations)
                   "?word?number" -> "word" (return word, if number == 1)
                   "?number" or "??number" -> "" (remove number,
                                                  if number == 1)
                   "?word?number" -> "number" (if number != 1)

            """
            def sub_tuple(m):
                """ word[number], !word[number], !!word[number], !!!word[number]
                    word, !word, !!word, !!!word, ?word?number, ??number, ?number
                    ?word?word[number], ?word?[number], ??word[number]
                """
                w, i = m.group('w', 'i')
                c = w[0]
                if c not in '!?':
                    return self.plural(w, symbols[int(i or 0)])
                elif c == '?':
                    (p1, sep, p2) = w[1:].partition("?")
                    part1 = p1 if sep else ""
                    (part2, sep, part3) = (p2 if sep else p1).partition("?")
                    if not sep:
                        part3 = part2
                    if i is None:
                        # ?[word]?number[?number] or ?number
                        if not part2:
                            return m.group(0)
                        num = int(part2)
                    else:
                        # ?[word]?word2[?word3][number]
                        num = int(symbols[int(i or 0)])
                    return part1 if num == 1 else part3 if num == 0 else part2
                elif w.startswith('!!!'):
                    word = w[3:]
                    fun = upper_fun
                elif w.startswith('!!'):
                    word = w[2:]
                    fun = title_fun
                else:
                    word = w[1:]
                    fun = cap_fun
                if i is not None:
                    return fun(self.plural(word, symbols[int(i)]))
                return fun(word)

            def sub_dict(m):
                """ word(var), !word(var), !!word(var), !!!word(var)
                    word(num), !word(num), !!word(num), !!!word(num)
                    ?word2(var), ?word1?word2(var), ?word1?word2?word0(var)
                    ?word2(num), ?word1?word2(num), ?word1?word2?word0(num)
                """
                w, n = m.group('w', 'n')
                c = w[0]
                n = int(n) if n.isdigit() else symbols[n]
                if c not in '!?':
                    return self.plural(w, n)
                elif c == '?':
                    # ?[word1]?word2[?word0](var or num), ?[word1]?word2(var or num) or ?word2(var or num)
                    (p1, sep, p2) = w[1:].partition("?")
                    part1 = p1 if sep else ""
                    (part2, sep, part3) = (p2 if sep else p1).partition("?")
                    if not sep:
                        part3 = part2
                    num = int(n)
                    return part1 if num == 1 else part3 if num == 0 else part2
                elif w.startswith('!!!'):
                    word = w[3:]
                    fun = upper_fun
                elif w.startswith('!!'):
                    word = w[2:]
                    fun = title_fun
                else:
                    word = w[1:]
                    fun = cap_fun
                return fun(self.plural(word, n))

            s = m.group(1)
            part = regex_plural_tuple.sub(sub_tuple, s)
            if part == s:
                part = regex_plural_dict.sub(sub_dict, s)
                if part == s:
                    return m.group(0)
            return part
        message = message % symbols
        message = regex_plural.sub(sub_plural, message)
        return message

    def translate(self, message, symbols):
        """
        Gets cached translated message with inserted parameters(symbols)
        """
        message = get_from_cache(self.cache, message,
                                 lambda: self.get_t(message))
        if symbols or symbols == 0 or symbols == "":
            if isinstance(symbols, dict):
                symbols.update(
                    (key, str(value).translate(ttab_in))
                    for key, value in symbols.iteritems()
                    if not isinstance(value, NUMBERS))
            else:
                if not isinstance(symbols, tuple):
                    symbols = (symbols,)
                symbols = tuple(
                    value if isinstance(value, NUMBERS)
                    else str(value).translate(ttab_in)
                    for value in symbols)
            message = self.params_substitution(message, symbols)
        return message.translate(ttab_out)


def findT(path, language=DEFAULT_LANGUAGE):
    """
    Note:
        Must be run by the admin app
    """
    lang_file = pjoin(path, 'languages', language + '.py')
    sentences = read_dict(lang_file)
    mp = pjoin(path, 'models')
    cp = pjoin(path, 'controllers')
    vp = pjoin(path, 'views')
    mop = pjoin(path, 'modules')
    for filename in \
            listdir(mp, '^.+\.py$', 0) + listdir(cp, '^.+\.py$', 0)\
            + listdir(vp, '^.+\.html$', 0) + listdir(mop, '^.+\.py$', 0):
        data = read_locked(filename)
        items = regex_translate.findall(data)
        for item in items:
            try:
                message = safe_eval(item)
            except:
                continue  # silently ignore inproperly formatted strings
            if not message.startswith('#') and not '\n' in message:
                tokens = message.rsplit('##', 1)
            else:
                # this allows markmin syntax in translations
                tokens = [message]
            if len(tokens) == 2:
                message = tokens[0].strip() + '##' + tokens[1].strip()
            if message and not message in sentences:
                sentences[message] = message
    if not '!langcode!' in sentences:
        sentences['!langcode!'] = (
            DEFAULT_LANGUAGE if language in ('default', DEFAULT_LANGUAGE) else language)
    if not '!langname!' in sentences:
        sentences['!langname!'] = (
            DEFAULT_LANGUAGE_NAME if language in ('default', DEFAULT_LANGUAGE)
            else sentences['!langcode!'])
    write_dict(lang_file, sentences)


def update_all_languages(application_path):
    """
    Note:
        Must be run by the admin app
    """
    path = pjoin(application_path, 'languages/')
    for language in oslistdir(path):
        if regex_langfile.match(language):
            findT(application_path, language[:-3])


if __name__ == '__main__':
    import doctest
    doctest.testmod()
