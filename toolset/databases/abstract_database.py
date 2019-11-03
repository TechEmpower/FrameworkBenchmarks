import abc
import re
import shlex
import subprocess
from toolset.utils.popen import PopenTimeout

class AbstractDatabase:
    '''
    Abstract Database Class.
    To be derived for defining a new concrete Database type
    '''
    #margin of tolerance on the results (rows read or updated only)
    margin = 1.011
    
    @classmethod
    @abc.abstractmethod
    def get_connection(cls, config):
        '''
        Establishes and returns a connection to the database.
        '''
        pass

    @classmethod
    @abc.abstractmethod
    def get_current_world_table(cls, config):
        '''
        Returns a JSON object containing all 10,000 World items as they currently
        exist in the database. This is used for verifying that entries in the
        database have actually changed during an Update verification test.
        '''
        pass

    @classmethod
    @abc.abstractmethod
    def test_connection(cls, config):
        '''
        Tests the connection and returns true if it is established.
        '''
        pass

    @classmethod
    @abc.abstractmethod
    def get_queries(cls, config):
        '''
        Returns the number of db queries (all types).
        '''
        pass

    @classmethod
    @abc.abstractmethod
    def get_rows(cls, config):
        '''
        Returns the number of db rows read.
        '''
        pass

    @classmethod
    @abc.abstractmethod
    def get_rows_updated(cls, config):
        '''
        Return the number of updated db rows.
        '''
        pass

    @classmethod
    @abc.abstractmethod
    def reset_cache(cls, config):
        '''
        Reset the query cache.
        '''
        pass

    @classmethod
    def verify_queries(cls, config, table_name, url, concurrency=512, count=2, check_updates=False):
        '''
        Verify query and row numbers for table_name.
        Retrieve from the database statistics of the number of queries made, the number of rows read, eventually the number of updated rows.
        Run 2 repetitions of http requests at the concurrency level 512 with siege.
        Retrieve statistics again, calculate the number of queries made and the number of rows read.
        '''
        trans_failures = 0
        rows_updated = None
        cls.tbl_name = table_name # used for Postgres and mongodb

        queries = int(cls.get_queries(config))
        rows = int(cls.get_rows(config))
        if check_updates:
            rows_updated = int(cls.get_rows_updated(config))

        cls.reset_cache(config)
        #Start siege requests with timeout (20s)
        path = config.db_root
        process = PopenTimeout(shlex.split("siege -c %s -r %s %s -R %s/.siegerc" % (concurrency, count, url, path)), stdout = subprocess.PIPE, stderr = subprocess.STDOUT, timeout=20)
        output, _ = process.communicate()
        #Search for failed transactions
        match = re.search('Failed transactions:.*?(\d+)\n', output, re.MULTILINE)
        if match:
            trans_failures = int(match.group(1))
            print output
        else:
            trans_failures = concurrency * count#Failed transactions: 100%

        queries = int(cls.get_queries(config)) - queries
        rows = int(cls.get_rows(config)) - rows
        if check_updates:
            rows_updated = int(cls.get_rows_updated(config)) - rows_updated

        return queries, rows, rows_updated, cls.margin, trans_failures
