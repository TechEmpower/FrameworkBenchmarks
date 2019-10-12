import abc
import os

class AbstractDatabase:
    '''
    Abstract Database Class
    '''
    @classmethod
    def verify_queries(cls, config, table_name, url, concurrency=512, count=15000):
        '''
        Verify queries and rows for table_name
        '''
        cls.tbl_name=table_name # only for Postgres
        queries=int(cls.get_queries(cls, config))
        rows=int(cls.get_rows(cls, config))
        cls.reset_cache(cls, config)
        
        os.system("ab -k -c %s -n %s %s" % (concurrency, count, url))
        
        queries=int(cls.get_queries(cls, config))-queries
        rows=int(cls.get_rows(cls, config))-rows
        return queries, rows
