from abc import ABC
import os

class AbstractDatabase(ABC):
    '''
    Abstract Database Class
    '''
    @classmethod
    @abstractmethod
    def get_connection(cls, config):

    @classmethod
    def verify_queries(cls, config, table_name, url, concurrency=512, count=15000):
        '''
        Verify queries and rows for table_name
        '''
        cls.tbl_name=table_name # only for Postgres
        queries=int(cls.get_queries(config))
        rows=int(cls.get_rows(config))
        cls.reset_cache(config)
        
        os.system("ab -k -c %s -n %s %s" % (concurrency, count, url))
        
        queries=int(cls.get_queries(config))-queries
        rows=int(cls.get_rows(config))-rows
        return queries, rows
