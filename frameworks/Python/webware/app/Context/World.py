from DbSession import Database
from sqlalchemy import Column
from sqlalchemy.types import Integer

class World(Database.Base):
    __tablename__ = "World"
    id = Column(Integer, primary_key=True)
    randomNumber = Column(Integer)
    def serialize(self):
        return {
            'id': self.id,
            'randomNumber': self.randomNumber,
        }