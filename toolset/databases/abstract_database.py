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
        cls.table_name=table_name
        queries=int(cls.get_queries(config))
        rows=int(cls.get_rows(config))
        cls.reset_cache(config)
        
        os.system("ab -k -c "+ str(concurrency) +" -n "+ str(count) +" "+ url)
        
        queries=int(cls.get_queries(config))-queries
        rows=int(cls.get_rows(config))-rows
        return queries, rows
