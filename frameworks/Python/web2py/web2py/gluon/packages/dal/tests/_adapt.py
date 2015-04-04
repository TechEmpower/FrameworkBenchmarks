import os

DEFAULT_URI = os.getenv('DB', 'sqlite:memory')
NOSQL = any([name in DEFAULT_URI for name in ("datastore", "mongodb", "imap")])
IS_IMAP = "imap" in DEFAULT_URI
IS_GAE = "datastore" in DEFAULT_URI
IS_MONGODB = "mongodb" in DEFAULT_URI
IS_POSTGRESQL = 'postgres' in DEFAULT_URI

def drop(table, cascade=None):
    if NOSQL and not (IS_MONGODB):
        # GAE drop/cleanup is not implemented
        db = table._db
        db[table]._common_filter = None
        db(table).delete()
        del db[table._tablename]
        del db.tables[db.tables.index(table._tablename)]
        db._remove_references_to(table)
    else:
        if cascade:
            table.drop(cascade)
        else:
            table.drop()
