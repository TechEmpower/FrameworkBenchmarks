#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Create web2py model (python code) to represent PostgreSQL tables.

Features:

* Uses ANSI Standard INFORMATION_SCHEMA (might work with other RDBMS)
* Detects legacy "keyed" tables (not having an "id" PK)
* Connects directly to running databases, no need to do a SQL dump
* Handles notnull, unique and referential constraints
* Detects most common datatypes and default values
* Support PostgreSQL columns comments (ie. for documentation)

Requeriments:

* Needs PostgreSQL pyscopg2 python connector (same as web2py)
* If used against other RDBMS, import and use proper connector (remove pg_ code)


Created by Mariano Reingart, based on a script to "generate schemas from dbs"
(mysql) by Alexandre Andrade

"""

_author__ = "Mariano Reingart <reingart@gmail.com>"

HELP = """
USAGE: extract_pgsql_models db host port user passwd

Call with PostgreSQL database connection parameters,
web2py model will be printed on standard output.

EXAMPLE: python extract_pgsql_models.py mydb localhost 5432 reingart saraza
"""

# Config options
DEBUG = False       # print debug messages to STDERR
SCHEMA = 'public'   # change if not using default PostgreSQL schema

# Constant for Field keyword parameter order (and filter):
KWARGS = ('type', 'length', 'default', 'required', 'ondelete',
          'notnull', 'unique', 'label', 'comment')


import sys


def query(conn, sql, *args):
    "Execute a SQL query and return rows as a list of dicts"
    cur = conn.cursor()
    ret = []
    try:
        if DEBUG:
            print >> sys.stderr, "QUERY: ", sql % args
        cur.execute(sql, args)
        for row in cur:
            dic = {}
            for i, value in enumerate(row):
                field = cur.description[i][0]
                dic[field] = value
            if DEBUG:
                print >> sys.stderr, "RET: ", dic
            ret.append(dic)
        return ret
    finally:
        cur.close()


def get_tables(conn, schema=SCHEMA):
    "List table names in a given schema"
    rows = query(conn, """SELECT table_name FROM information_schema.tables
        WHERE table_schema = %s
        ORDER BY table_name""", schema)
    return [row['table_name'] for row in rows]


def get_fields(conn, table):
    "Retrieve field list for a given table"
    if DEBUG:
        print >> sys.stderr, "Processing TABLE", table
    rows = query(conn, """
        SELECT column_name, data_type,
            is_nullable,
            character_maximum_length,
            numeric_precision, numeric_precision_radix, numeric_scale,
            column_default
        FROM information_schema.columns
        WHERE table_name=%s
        ORDER BY ordinal_position""", table)
    return rows


def define_field(conn, table, field, pks):
    "Determine field type, default value, references, etc."
    f = {}
    ref = references(conn, table, field['column_name'])
    if ref:
        f.update(ref)
    elif field['column_default'] and \
        field['column_default'].startswith("nextval") and \
            field['column_name'] in pks:
        # postgresql sequence (SERIAL) and primary key!
        f['type'] = "'id'"
    elif field['data_type'].startswith('character'):
        f['type'] = "'string'"
        if field['character_maximum_length']:
            f['length'] = field['character_maximum_length']
    elif field['data_type'] in ('text', ):
        f['type'] = "'text'"
    elif field['data_type'] in ('boolean', 'bit'):
        f['type'] = "'boolean'"
    elif field['data_type'] in ('integer', 'smallint', 'bigint'):
        f['type'] = "'integer'"
    elif field['data_type'] in ('double precision', 'real'):
        f['type'] = "'double'"
    elif field['data_type'] in ('timestamp', 'timestamp without time zone'):
        f['type'] = "'datetime'"
    elif field['data_type'] in ('date', ):
        f['type'] = "'date'"
    elif field['data_type'] in ('time', 'time without time zone'):
        f['type'] = "'time'"
    elif field['data_type'] in ('numeric', 'currency'):
        f['precision'] = field['numeric_precision']
        f['scale'] = field['numeric_scale'] or 0
        f['type'] = "'decimal({},{})'".format(f['precision'],f['scale'])
    elif field['data_type'] in ('bytea', ):
        f['type'] = "'blob'"
    elif field['data_type'] in ('point', 'lseg', 'polygon', 'unknown', 'USER-DEFINED'):
        f['type'] = ""  # unsupported?
    else:
        raise RuntimeError("Data Type not supported: %s " % str(field))

    try:
        if field['column_default']:
            if field['column_default'] == "now()":
                d = "request.now"
            elif field['column_default'] == "true":
                d = "True"
            elif field['column_default'] == "false":
                d = "False"
            else:
                d = repr(eval(field['column_default']))
            f['default'] = str(d)
    except (ValueError, SyntaxError):
        pass
    except Exception, e:
        raise RuntimeError(
            "Default unsupported '%s'" % field['column_default'])

    if not field['is_nullable']:
        f['notnull'] = "True"

    comment = get_comment(conn, table, field)
    if comment is not None:
        f['comment'] = repr(comment)
    return f


def is_unique(conn, table, field):
    "Find unique columns (incomplete support)"
    rows = query(conn, """
        SELECT information_schema.constraint_column_usage.column_name
        FROM information_schema.table_constraints
        NATURAL JOIN information_schema.constraint_column_usage
        WHERE information_schema.table_constraints.table_name=%s
          AND information_schema.constraint_column_usage.column_name=%s
          AND information_schema.table_constraints.constraint_type='UNIQUE'
        ;""", table, field['column_name'])
    return rows and True or False


def get_comment(conn, table, field):
    "Find the column comment (postgres specific)"
    rows = query(conn, """
        SELECT d.description AS comment
        FROM pg_class c
        JOIN pg_description d ON c.oid=d.objoid
        JOIN pg_attribute a ON c.oid = a.attrelid
        WHERE c.relname=%s AND a.attname=%s
        AND a.attnum = d.objsubid
        ;""", table, field['column_name'])
    return rows and rows[0]['comment'] or None


def primarykeys(conn, table):
    "Find primary keys"
    rows = query(conn, """
        SELECT information_schema.constraint_column_usage.column_name
        FROM information_schema.table_constraints
        NATURAL JOIN information_schema.constraint_column_usage
        WHERE information_schema.table_constraints.table_name=%s
          AND information_schema.table_constraints.constraint_type='PRIMARY KEY'
        ;""", table)
    return [row['column_name'] for row in rows]


def references(conn, table, field):
    "Find a FK (fails if multiple)"
    rows1 = query(conn, """
        SELECT table_name, column_name, constraint_name,
               update_rule, delete_rule, ordinal_position
        FROM information_schema.key_column_usage
        NATURAL JOIN information_schema.referential_constraints
        NATURAL JOIN information_schema.table_constraints
        WHERE information_schema.key_column_usage.table_name=%s
          AND information_schema.key_column_usage.column_name=%s
          AND information_schema.table_constraints.constraint_type='FOREIGN KEY'
          ;""", table, field)
    if len(rows1) == 1:
        rows2 = query(conn, """
            SELECT table_name, column_name, *
            FROM information_schema.constraint_column_usage
            WHERE constraint_name=%s
            """, rows1[0]['constraint_name'])
        row = None
        if len(rows2) > 1:
            row = rows2[int(rows1[0]['ordinal_position']) - 1]
            keyed = True
        if len(rows2) == 1:
            row = rows2[0]
            keyed = False
        if row:
            if keyed:  # THIS IS BAD, DON'T MIX "id" and primarykey!!!
                ref = {'type': "'reference %s.%s'" % (row['table_name'],
                                                      row['column_name'])}
            else:
                ref = {'type': "'reference %s'" % (row['table_name'],)}
            if rows1[0]['delete_rule'] != "NO ACTION":
                ref['ondelete'] = repr(rows1[0]['delete_rule'])
            return ref
        elif rows2:
            raise RuntimeError("Unsupported foreign key reference: %s" %
                               str(rows2))

    elif rows1:
        raise RuntimeError("Unsupported referential constraint: %s" %
                           str(rows1))


def define_table(conn, table):
    "Output single table definition"
    fields = get_fields(conn, table)
    pks = primarykeys(conn, table)
    print "db.define_table('%s'," % (table, )
    for field in fields:
        fname = field['column_name']
        fdef = define_field(conn, table, field, pks)
        if fname not in pks and is_unique(conn, table, field):
            fdef['unique'] = "True"
        if fdef['type'] == "'id'" and fname in pks:
            pks.pop(pks.index(fname))
        print "    Field('%s', %s)," % (fname,
                                        ', '.join(["%s=%s" % (k, fdef[k]) for k in KWARGS
                                                   if k in fdef and fdef[k]]))
    if pks:
        print "    primarykey=[%s]," % ", ".join(["'%s'" % pk for pk in pks])
    print     "    migrate=migrate)"
    print


def define_db(conn, db, host, port, user, passwd):
    "Output database definition (model)"
    dal = 'db = DAL("postgres://%s:%s@%s:%s/%s", pool_size=10)'
    print dal % (user, passwd, host, port, db)
    print
    print "migrate = False"
    print
    for table in get_tables(conn):
        define_table(conn, table)


if __name__ == "__main__":
    if len(sys.argv) < 6:
        print HELP
    else:
        # Parse arguments from command line:
        db, host, port, user, passwd = sys.argv[1:6]

        # Make the database connection (change driver if required)
        import psycopg2
        cnn = psycopg2.connect(database=db, host=host, port=port,
                               user=user, password=passwd,
                               )
        # Start model code generation:
        define_db(cnn, db, host, port, user, passwd)
