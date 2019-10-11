import abc
import os

class AbstractDatabase:
    '''
    Abstract Database Class
    '''
     @classmethod
    def verify_queries(cls, table_name, url, concurrency=512, count=15000):
        '''
        Verify queries and rows for table_name
        '''
        cls.table_name=table_name
        queries=int(cls.get_queries())
        rows=int(cls.get_rows())
        cls.reset_cache()
        
        os.system("ab -k -c "+ str(concurrency) +" -n "+ str(count) +" "+ url)
        
        queries=int(cls.get_queries())-queries
        rows=int(cls.get_rows())-rows
        return queries, rows
