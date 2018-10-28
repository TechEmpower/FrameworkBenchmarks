import os

from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

DBDRIVER = 'mysql'
DBHOSTNAME = 'tfb-database'
DATABASE_URI = '%s://benchmarkdbuser:benchmarkdbpass@%s:3306/hello_world?charset=utf8' % (DBDRIVER, DBHOSTNAME)

class Database():
	Base = declarative_base()
	db_engine = create_engine(DATABASE_URI)
	Session = sessionmaker(bind=db_engine)
	DbSession = Session()
