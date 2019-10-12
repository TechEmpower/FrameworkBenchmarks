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
        queries=int(cls.get_queries(config, table_name))
        rows=int(cls.get_rows(config, table_name))
        cls.reset_cache(config)
        
        os.system("ab -k -c %s -n %s %s" % (concurrency, count, url))
        
        queries=int(cls.get_queries(config, table_name))-queries
        rows=int(cls.get_rows(config, table_name))-rows
        return queries, rows
