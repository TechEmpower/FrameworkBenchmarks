import codecs
from frameworkbenchmarks.models import DBSession

if __name__ == "__main__":
    """
    Initialize database
    """
    with codecs.open('../config/create-postgres.sql', 'r', encoding='utf-8') as fp:
        sql = fp.read()
    DBSession.execute(sql)
    DBSession.commit()


