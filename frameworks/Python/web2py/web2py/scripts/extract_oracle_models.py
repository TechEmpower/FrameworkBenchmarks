#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Create web2py model (python code) to represent Oracle 11g tables.

Features:

* Uses Oracle's metadata tables
* Detects legacy "keyed" tables (not having an "id" PK)
* Connects directly to running databases, no need to do a SQL dump
* Handles notnull, unique and referential constraints
* Detects most common datatypes and default values
* Documents alternative datatypes as comments

Requeriments:

* Needs Oracle cx_Oracle python connector (same as web2py)


Created by Oscar Fonts, based on extract_pgsql_models by Mariano Reingart,
based in turn on a script to "generate schemas from dbs" (mysql)
by Alexandre Andrade

"""

_author__ = "Oscar Fonts <oscar.fonts@geomati.co>"

HELP = """
USAGE: extract_oracle_models db host port user passwd

Call with Oracle database connection parameters,
web2py model will be printed on standard output.

EXAMPLE: python extract_oracle_models.py ORCL localhost 1521 user password
"""

# Config options
DEBUG = False       # print debug messages to STDERR

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
            print >> sys.stderr, "QUERY: ", sql , args
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
    except cx_Oracle.DatabaseError, exc:
        error, = exc.args
        print >> sys.stderr, "Oracle-Error-Message:", error.message
    finally:
        cur.close()


def get_tables(conn):
    "List table names in a given schema"
    rows = query(conn, """SELECT TABLE_NAME FROM USER_TABLES
        ORDER BY TABLE_NAME""")
    return [row['TABLE_NAME'] for row in rows]


def get_fields(conn, table):
    "Retrieve field list for a given table"
    if DEBUG:
        print >> sys.stderr, "Processing TABLE", table
    rows = query(conn, """
        SELECT COLUMN_NAME, DATA_TYPE,
            NULLABLE AS IS_NULLABLE,
            CHAR_LENGTH AS CHARACTER_MAXIMUM_LENGTH,
            DATA_PRECISION AS NUMERIC_PRECISION,
            DATA_SCALE AS NUMERIC_SCALE,
            DATA_DEFAULT AS COLUMN_DEFAULT
        FROM USER_TAB_COLUMNS
        WHERE TABLE_NAME=:t
        """, table)

    return rows


def define_field(conn, table, field, pks):
    "Determine field type, default value, references, etc."
    f = {}
    ref = references(conn, table, field['COLUMN_NAME'])
    # Foreign Keys
    if ref:
        f.update(ref)
    # PK & Numeric & autoincrement => id
    elif field['COLUMN_NAME'] in pks and \
            field['DATA_TYPE'] in ('INT', 'NUMBER') and \
            is_autoincrement(conn, table, field):
        f['type'] = "'id'"
    # Other data types
    elif field['DATA_TYPE'] in ('BINARY_DOUBLE'):
        f['type'] = "'double'"
    elif field['DATA_TYPE'] in ('CHAR','NCHAR'):
        f['type'] = "'string'"
        f['comment'] = "'Alternative types: boolean, time'"
    elif field['DATA_TYPE'] in ('BLOB', 'CLOB'):
        f['type'] = "'blob'"
        f['comment'] = "'Alternative types: text, json, list:*'"
    elif field['DATA_TYPE'] in ('DATE'):
        f['type'] = "'datetime'"
        f['comment'] = "'Alternative types: date'"
    elif field['DATA_TYPE'] in ('FLOAT'):
        f['type'] = "'float'"
    elif field['DATA_TYPE'] in ('INT'):
        f['type'] = "'integer'"
    elif field['DATA_TYPE'] in ('NUMBER'):
        f['type'] = "'bigint'"
    elif field['DATA_TYPE'] in ('NUMERIC'):
        f['type'] = "'decimal'"
        f['precision'] = field['NUMERIC_PRECISION']
        f['scale'] = field['NUMERIC_SCALE'] or 0
    elif field['DATA_TYPE'] in ('VARCHAR2','NVARCHAR2'):
        f['type'] = "'string'"
        if field['CHARACTER_MAXIMUM_LENGTH']:
            f['length'] = field['CHARACTER_MAXIMUM_LENGTH']
        f['comment'] = "'Other possible types: password, upload'"
    else:
        f['type'] = "'blob'"
        f['comment'] = "'WARNING: Oracle Data Type %s was not mapped." % \
                str(field['DATA_TYPE']) + " Using 'blob' as fallback.'"

    try:
        if field['COLUMN_DEFAULT']:
            if field['COLUMN_DEFAULT'] == "sysdate":
                d = "request.now"
            elif field['COLUMN_DEFAULT'].upper() == "T":
                d = "True"
            elif field['COLUMN_DEFAULT'].upper() == "F":
                d = "False"
            else:
                d = repr(eval(field['COLUMN_DEFAULT']))
            f['default'] = str(d)
    except (ValueError, SyntaxError):
        pass
    except Exception, e:
        raise RuntimeError(
            "Default unsupported '%s'" % field['COLUMN_DEFAULT'])

    if not field['IS_NULLABLE']:
        f['notnull'] = "True"

    return f


def is_unique(conn, table, field):
    "Find unique columns"
    rows = query(conn, """
        SELECT COLS.COLUMN_NAME
        FROM USER_CONSTRAINTS CONS, ALL_CONS_COLUMNS COLS
        WHERE CONS.OWNER = COLS.OWNER
          AND CONS.CONSTRAINT_NAME = COLS.CONSTRAINT_NAME
          AND CONS.CONSTRAINT_TYPE = 'U'
          AND COLS.TABLE_NAME = :t
          AND COLS.COLUMN_NAME = :c
        """, table, field['COLUMN_NAME'])
    return rows and True or False


# Returns True when a "BEFORE EACH ROW INSERT" trigger is found and:
#  a) it mentions the "NEXTVAL" keyword (used by sequences)
#  b) it operates on the given table and column
#
# On some (inelegant) database designs, SEQUENCE.NEXTVAL is called directly
# from each "insert" statement, instead of using triggers. Such cases cannot
# be detected by inspecting Oracle's metadata tables, as sequences are not
# logically bound to any specific table or field.
def is_autoincrement(conn, table, field):
    "Find auto increment fields (best effort)"
    rows = query(conn, """
        SELECT TRIGGER_NAME
        FROM USER_TRIGGERS,
          (SELECT NAME, LISTAGG(TEXT, ' ') WITHIN GROUP (ORDER BY LINE) TEXT
           FROM USER_SOURCE
           WHERE TYPE = 'TRIGGER'
           GROUP BY NAME
          ) TRIGGER_DEFINITION
        WHERE TRIGGER_NAME = NAME
          AND TRIGGERING_EVENT = 'INSERT'
          AND TRIGGER_TYPE = 'BEFORE EACH ROW'
          AND TABLE_NAME = :t
          AND UPPER(TEXT) LIKE UPPER('%.NEXTVAL%')
          AND UPPER(TEXT) LIKE UPPER('%:NEW.' || :c || '%')
        """, table, field['COLUMN_NAME'])
    return rows and True or False


def primarykeys(conn, table):
    "Find primary keys"
    rows = query(conn, """
        SELECT COLS.COLUMN_NAME
        FROM USER_CONSTRAINTS CONS, ALL_CONS_COLUMNS COLS
        WHERE COLS.TABLE_NAME = :t
            AND CONS.CONSTRAINT_TYPE = 'P'
            AND CONS.OWNER = COLS.OWNER
            AND CONS.CONSTRAINT_NAME = COLS.CONSTRAINT_NAME
        """, table)

    return [row['COLUMN_NAME'] for row in rows]


def references(conn, table, field):
    "Find a FK (fails if multiple)"
    rows1 = query(conn, """
        SELECT COLS.CONSTRAINT_NAME,
            CONS.DELETE_RULE,
            COLS.POSITION AS ORDINAL_POSITION
        FROM USER_CONSTRAINTS CONS, ALL_CONS_COLUMNS COLS
        WHERE COLS.TABLE_NAME = :t
            AND COLS.COLUMN_NAME = :c
            AND CONS.CONSTRAINT_TYPE = 'R'
            AND CONS.OWNER = COLS.OWNER
            AND CONS.CONSTRAINT_NAME = COLS.CONSTRAINT_NAME
          """, table, field)

    if len(rows1) == 1:
        rows2 = query(conn, """
                SELECT COLS.TABLE_NAME, COLS.COLUMN_NAME
                FROM USER_CONSTRAINTS CONS, ALL_CONS_COLUMNS COLS
                WHERE CONS.CONSTRAINT_NAME = :k
                   AND CONS.R_CONSTRAINT_NAME = COLS.CONSTRAINT_NAME
                ORDER BY COLS.POSITION ASC
            """, rows1[0]['CONSTRAINT_NAME'])

        row = None
        if len(rows2) > 1:
            row = rows2[int(rows1[0]['ORDINAL_POSITION']) - 1]
            keyed = True
        if len(rows2) == 1:
            row = rows2[0]
            keyed = False
        if row:
            if keyed:  # THIS IS BAD, DON'T MIX "id" and primarykey!!!
                ref = {'type': "'reference %s.%s'" % (row['TABLE_NAME'],
                                                      row['COLUMN_NAME'])}
            else:
                ref = {'type': "'reference %s'" % (row['TABLE_NAME'],)}
            if rows1[0]['DELETE_RULE'] != "NO ACTION":
                ref['ondelete'] = repr(rows1[0]['DELETE_RULE'])
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
        fname = field['COLUMN_NAME']
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
    dal = 'db = DAL("oracle://%s/%s@%s:%s/%s", pool_size=10)'
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
        import cx_Oracle
        dsn = cx_Oracle.makedsn(host, port, db)
        cnn = cx_Oracle.connect(user, passwd, dsn)
        # Start model code generation:
        define_db(cnn, db, host, port, user, passwd)
