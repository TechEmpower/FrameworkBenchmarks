from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import Column
from sqlalchemy.types import String, Integer

Base = declarative_base()

class World(Base):
    __tablename__ = "world"

    id = Column(Integer, primary_key = True)
    randomNumber = Column(Integer)

    def serialize(self):
        return {
            'id' : int(self.id),
            'randomNumber' : int(self.randomNumber)
        }
