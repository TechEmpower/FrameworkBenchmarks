from DbSession import Database
from sqlalchemy import Column
from sqlalchemy.types import Integer, String

class AFortune(Database.Base):
    __tablename__ = "Fortune"
    id = Column(Integer, primary_key=True)
    message = Column(String)

    def serialize(self):
        return {
            'id': self.id,
            'randomNumber': self.randomNumber,
        }