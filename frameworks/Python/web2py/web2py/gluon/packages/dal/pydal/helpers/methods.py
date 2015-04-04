# -*- coding: utf-8 -*-
import uuid
import re

from .regex import REGEX_NOPASSWD, REGEX_UNPACK, REGEX_CONST_STRING, REGEX_W
from .classes import SQLCustomType
#from ..objects import Field, Table


PLURALIZE_RULES = [
    (re.compile('child$'), re.compile('child$'), 'children'),
    (re.compile('oot$'), re.compile('oot$'), 'eet'),
    (re.compile('ooth$'), re.compile('ooth$'), 'eeth'),
    (re.compile('l[eo]af$'), re.compile('l([eo])af$'), 'l\\1aves'),
    (re.compile('sis$'), re.compile('sis$'), 'ses'),
    (re.compile('man$'), re.compile('man$'), 'men'),
    (re.compile('ife$'), re.compile('ife$'), 'ives'),
    (re.compile('eau$'), re.compile('eau$'), 'eaux'),
    (re.compile('lf$'), re.compile('lf$'), 'lves'),
    (re.compile('[sxz]$'), re.compile('$'), 'es'),
    (re.compile('[^aeioudgkprt]h$'), re.compile('$'), 'es'),
    (re.compile('(qu|[^aeiou])y$'), re.compile('y$'), 'ies'),
    (re.compile('$'), re.compile('$'), 's'),
    ]

def pluralize(singular, rules=PLURALIZE_RULES):
    for line in rules:
        re_search, re_sub, replace = line
        plural = re_search.search(singular) and re_sub.sub(replace, singular)
        if plural: return plural

def hide_password(uri):
    if isinstance(uri,(list,tuple)):
        return [hide_password(item) for item in uri]
    return REGEX_NOPASSWD.sub('******',uri)


def cleanup(text):
    """
    Validates that the given text is clean: only contains [0-9a-zA-Z_]
    """
    #if not REGEX_ALPHANUMERIC.match(text):
    #    raise SyntaxError('invalid table or field name: %s' % text)
    return text


def list_represent(values, row=None):
    return ', '.join(str(v) for v in (values or []))


def xorify(orderby):
    if not orderby:
        return None
    orderby2 = orderby[0]
    for item in orderby[1:]:
        orderby2 = orderby2 | item
    return orderby2


def use_common_filters(query):
    return (query and hasattr(query,'ignore_common_filters') and \
                not query.ignore_common_filters)


def bar_escape(item):
    return str(item).replace('|', '||')


def bar_encode(items):
    return '|%s|' % '|'.join(bar_escape(item) for item in items if str(item).strip())


def bar_decode_integer(value):
    if not hasattr(value,'split') and hasattr(value,'read'):
        value = value.read()
    return [long(x) for x in value.split('|') if x.strip()]


def bar_decode_string(value):
    return [x.replace('||', '|') for x in
            REGEX_UNPACK.split(value[1:-1]) if x.strip()]


def archive_record(qset, fs, archive_table, current_record):
    tablenames = qset.db._adapter.tables(qset.query)
    if len(tablenames) != 1:
        raise RuntimeError("cannot update join")
    for row in qset.select():
        fields = archive_table._filter_fields(row)
        for k, v in fs.iteritems():
            if fields[k] != v:
                fields[current_record] = row.id
                archive_table.insert(**fields)
                break
    return False


def smart_query(fields,text):
    from ..objects import Field, Table
    if not isinstance(fields,(list,tuple)):
        fields = [fields]
    new_fields = []
    for field in fields:
        if isinstance(field,Field):
            new_fields.append(field)
        elif isinstance(field,Table):
            for ofield in field:
                new_fields.append(ofield)
        else:
            raise RuntimeError("fields must be a list of fields")
    fields = new_fields
    field_map = {}
    for field in fields:
        n = field.name.lower()
        if not n in field_map:
            field_map[n] = field
        n = str(field).lower()
        if not n in field_map:
            field_map[n] = field
    constants = {}
    i = 0
    while True:
        m = REGEX_CONST_STRING.search(text)
        if not m: break
        text = text[:m.start()]+('#%i' % i)+text[m.end():]
        constants[str(i)] = m.group()[1:-1]
        i+=1
    text = re.sub('\s+',' ',text).lower()
    for a,b in [('&','and'),
                ('|','or'),
                ('~','not'),
                ('==','='),
                ('<','<'),
                ('>','>'),
                ('<=','<='),
                ('>=','>='),
                ('<>','!='),
                ('=<','<='),
                ('=>','>='),
                ('=','='),
                (' less or equal than ','<='),
                (' greater or equal than ','>='),
                (' equal or less than ','<='),
                (' equal or greater than ','>='),
                (' less or equal ','<='),
                (' greater or equal ','>='),
                (' equal or less ','<='),
                (' equal or greater ','>='),
                (' not equal to ','!='),
                (' not equal ','!='),
                (' equal to ','='),
                (' equal ','='),
                (' equals ','='),
                (' less than ','<'),
                (' greater than ','>'),
                (' starts with ','startswith'),
                (' ends with ','endswith'),
                (' not in ' , 'notbelongs'),
                (' in ' , 'belongs'),
                (' is ','=')]:
        if a[0]==' ':
            text = text.replace(' is'+a,' %s ' % b)
        text = text.replace(a,' %s ' % b)
    text = re.sub('\s+',' ',text).lower()
    text = re.sub('(?P<a>[\<\>\!\=])\s+(?P<b>[\<\>\!\=])','\g<a>\g<b>',text)
    query = field = neg = op = logic = None
    for item in text.split():
        if field is None:
            if item == 'not':
                neg = True
            elif not neg and not logic and item in ('and','or'):
                logic = item
            elif item in field_map:
                field = field_map[item]
            else:
                raise RuntimeError("Invalid syntax")
        elif not field is None and op is None:
            op = item
        elif not op is None:
            if item.startswith('#'):
                if not item[1:] in constants:
                    raise RuntimeError("Invalid syntax")
                value = constants[item[1:]]
            else:
                value = item
                if field.type in ('text', 'string', 'json'):
                    if op == '=': op = 'like'
            if op == '=': new_query = field==value
            elif op == '<': new_query = field<value
            elif op == '>': new_query = field>value
            elif op == '<=': new_query = field<=value
            elif op == '>=': new_query = field>=value
            elif op == '!=': new_query = field!=value
            elif op == 'belongs': new_query = field.belongs(value.split(','))
            elif op == 'notbelongs': new_query = ~field.belongs(value.split(','))
            elif field.type in ('text', 'string', 'json'):
                if op == 'contains': new_query = field.contains(value)
                elif op == 'like': new_query = field.ilike(value)
                elif op == 'startswith': new_query = field.startswith(value)
                elif op == 'endswith': new_query = field.endswith(value)
                else: raise RuntimeError("Invalid operation")
            elif field._db._adapter.dbengine=='google:datastore' and \
                 field.type in ('list:integer', 'list:string', 'list:reference'):
                if op == 'contains': new_query = field.contains(value)
                else: raise RuntimeError("Invalid operation")
            else: raise RuntimeError("Invalid operation")
            if neg: new_query = ~new_query
            if query is None:
                query = new_query
            elif logic == 'and':
                query &= new_query
            elif logic == 'or':
                query |= new_query
            field = op = neg = logic = None
    return query


def auto_validators(field):
    db = field.db
    field_type = field.type
    #: don't apply default validation on custom types
    if isinstance(field_type, SQLCustomType):
        if hasattr(field_type, 'validator'):
            return field_type.validator
        else:
            field_type = field_type.type
    elif not isinstance(field_type, str):
        return []
    #: if a custom method is provided, call it
    if callable(db.validators_method):
        return db.validators_method(field)
    #: apply validators from validators dict if present
    if not db.validators or not isinstance(db.validators, dict):
        return []
    field_validators = db.validators.get(field_type, [])
    if not isinstance(field_validators, (list, tuple)):
        field_validators = [field_validators]
    return field_validators


def _fieldformat(r, id):
    row = r(id)
    if not row:
        return str(id)
    elif hasattr(r, '_format') and isinstance(r._format, str):
        return r._format % row
    elif hasattr(r, '_format') and callable(r._format):
        return r._format(row)
    else:
        return str(id)


class _repr_ref(object):
    def __init__(self, ref=None):
        self.ref = ref

    def __call__(self, value, row=None):
        return _fieldformat(self.row, value)


class _repr_ref_list(_repr_ref):
    def __call__(self, value, row=None):
        if not value:
            return None
        from ..adapters import GoogleDatastoreAdapter
        refs = None
        db, id = self.ref._db, self.ref._id
        if isinstance(db._adapter, GoogleDatastoreAdapter):
            def count(values):
                return db(id.belongs(values)).select(id)
            rx = range(0, len(value), 30)
            refs = reduce(lambda a, b: a & b, [count(value[i:i+30])
                          for i in rx])
        else:
            refs = db(id.belongs(value)).select(id)
        return refs and ', '.join(
            _fieldformat(self.ref, x.id) for x in value) or ''


def auto_represent(field):
    if field.represent:
        return field.represent
    if field.db and field.type.startswith('reference') and \
            field.type.find('.') < 0 and field.type[10:] in field.db.tables:
        referenced = field.db[field.type[10:]]
        return _repr_ref(referenced)
    elif field.db and field.type.startswith('list:reference') and \
            field.type.find('.') < 0 and field.type[15:] in field.db.tables:
        referenced = field.db[field.type[15:]]
        return _repr_ref_list(referenced)
    return field.represent


def varquote_aux(name, quotestr='%s'):
    return name if REGEX_W.match(name) else quotestr % name


def uuid2int(uuidv):
    return uuid.UUID(uuidv).int


def int2uuid(n):
    return str(uuid.UUID(int=n))


# Geodal utils
def geoPoint(x, y):
    return "POINT (%f %f)" % (x, y)


def geoLine(*line):
    return "LINESTRING (%s)" % ','.join("%f %f" % item for item in line)


def geoPolygon(*line):
    return "POLYGON ((%s))" % ','.join("%f %f" % item for item in line)
