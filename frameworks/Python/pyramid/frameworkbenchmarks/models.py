"""
Benchmark models.
"""


from sqlalchemy import Column, Integer, MetaData, String, create_engine
from sqlalchemy.orm import declarative_base, sessionmaker
from sqlalchemy.pool import QueuePool


def get_engine(settings):
    return create_engine(
        settings["sqlalchemy.url"],
        poolclass=QueuePool,
        pool_size=100,
        max_overflow=25,
        enable_from_linting=False,
        future=True,
    )


def get_session_factory(engine):
    Session = sessionmaker(bind=engine, autoflush=False, future=True)
    return Session


metadata = MetaData()

Base = declarative_base()


class World(Base):
    __tablename__ = "world"

    id = Column("id", Integer, primary_key=True)
    randomNumber = Column("randomnumber", Integer, nullable=False, server_default="0")

    def __json__(self, request=None):
        return {"id": self.id, "randomNumber": self.randomNumber}


class Fortune(Base):
    __tablename__ = "fortune"

    id = Column("id", Integer, primary_key=True)
    message = Column("message", String, nullable=False)

    def __json__(self):
        return {"id": self.id, "message": self.message}
