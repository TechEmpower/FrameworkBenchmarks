import abc
import os

class AbstractDatabase:
    '''
    Abstract Database Class
    '''
    @classmethod
    @abc.abstractmethod
    def get_connection(cls, config):
        pass

    @classmethod
    @abc.abstractmethod
    def get_current_world_table(cls, config):
        pass

    @classmethod
    @abc.abstractmethod
    def test_connection(cls, config):
        pass

    @classmethod
    @abc.abstractmethod
    def get_queries(cls, config):
        pass

    @classmethod
    @abc.abstractmethod
    def get_rows(cls, config):
        pass

    @classmethod
    @abc.abstractmethod
    def get_rows_updated(cls, config):
        pass

    @classmethod
    @abc.abstractmethod
    def reset_cache(cls, config):
        pass

    @classmethod
    def verify_queries(cls, config, table_name, url, concurrency=512, count=15000, check_updates=False):
        '''
        Verify queries and rows for table_name
        '''
        rows_updated= None
        cls.tbl_name=table_name # only for Postgres
        queries=int(cls.get_queries(config))
        rows=int(cls.get_rows(config))
        if check_updates:
            rows_updated=int(cls.get_rows_updated(config))

        cls.reset_cache(config)
        
        os.system("siege -c %s -r %s %s -R<echo connection = keep-alive" % (concurrency, count, url))
        
        queries=int(cls.get_queries(config))-queries
        rows=int(cls.get_rows(config))-rows
        if check_updates:
            rows_updated=int(cls.get_rows_updated(config))-rows_updated

        return queries, rows, rows_updated
