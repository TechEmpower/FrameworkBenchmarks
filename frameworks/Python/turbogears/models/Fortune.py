from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import Column
from sqlalchemy.types import String, Integer

Base = declarative_base()

class Fortune(Base):
    __tablename__ = "fortune"

    id = Column(Integer, primary_key = True)
    message = Column(String)

    def serialize(self):
        return {
            'id' : int(self.id),
            'message' : self.message
        }
