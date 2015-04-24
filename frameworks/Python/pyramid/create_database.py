import codecs
import os
from frameworkbenchmarks.models import DBSession

FWROOT = os.environ.get('FWROOT', '../../..')

if __name__ == "__main__":
    """
    Initialize database
    """
    with codecs.open('%s/config/create-postgres.sql' % FWROOT,
                     'r',
                     encoding='utf-8') as fp:
        sql = fp.read()
    DBSession.execute(sql)
    DBSession.commit()


