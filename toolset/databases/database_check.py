import abc
import os

class DatabaseCheck:
    '''
    Abstract Class for checking Database connection and queries
    '''
    def __init__(self, config,tbl_name="world"):
        self.config = config
        self.tbl_name = tbl_name

    def run(self, url, concurrency=512, count=15000):
        '''
        Gets the number of queries and the number of rows read
        '''
        queries=int(self.get_queries())
        rows=int(self.get_rows())
        self.reset_cache()
        
        os.system("ab -k -c "+ str(concurrency) +" -n "+ str(count) +" "+ url)
        
        queries=int(self.get_queries())-queries
        rows=int(self.get_rows())-rows
        return queries, rows

    def connect(self):
        '''
        Establishes and returns a connection to the database
        '''
        pass

    def get_queries(self):
        '''
        Returns the number of queries executed
        '''
        pass

    def get_rows(self):
        '''
        Returns the number of rows read from the database
        '''
        pass

    def reset_cache(self):
        '''
        resets the queries cache
        '''
        pass
