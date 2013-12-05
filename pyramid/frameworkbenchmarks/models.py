"""
Benchmark models.
"""

import json
import psycopg2
from collections import Iterable
from sqlalchemy import create_engine, MetaData, Table, Column, Integer, String
from sqlalchemy.orm import sessionmaker
from sqlalchemy.pool import QueuePool
from sqlalchemy.ext.declarative import declarative_base, DeclarativeMeta

DBHOSTNAME = 'localhost'

def get_conn():
    return psycopg2.connect(
        user = 'benchmarkdbuser',
        password = 'benchmarkdbpass',
        host = DBHOSTNAME,
        port = '5432',
        database = 'hello_world'
        )

conn_pool = QueuePool(get_conn, pool_size=100, max_overflow=25, echo=False)

pg = create_engine('postgresql://', pool=conn_pool)
DBSession = sessionmaker(bind=pg)()
metadata = MetaData()

DatabaseBase = declarative_base()

def sqlalchemy_encoder_factory(system_values):
    return SQLAlchemyEncoder()


class SQLAlchemyEncoder(json.JSONEncoder):

    def __call__(self, obj, system_values):
        if isinstance(obj, Iterable):
            return json.dumps([self.default(x) for x in obj])
        else:
            return json.dumps(self.default(obj))

    def default(self, obj):
        if isinstance(obj.__class__, DeclarativeMeta):
            return obj.__json__()
        return super(SQLAlchemyEncoder, self).default(obj)


class World(DatabaseBase):
    __tablename__ = 'World'

    id = Column('id', Integer, primary_key=True)
    randomNumber = Column('randomnumber', Integer, nullable=False, server_default='0')

    def __json__(self):
        return {'id': self.id, 'randomnumber': self.randomNumber}


class Fortune(DatabaseBase):
    __tablename__ = 'Fortune'

    id = Column('id', Integer, primary_key=True)
    message = Column('message', String, nullable=False)

    def __json__(self):
        return {'id': self.id, 'message': self.message}
