from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column

class Base(DeclarativeBase):
    """Base for models."""


class World(Base):
    __tablename__ = 'world'
    id: Mapped[int] = mapped_column(primary_key=True)
    randomnumber: Mapped[int]

sa_worlds = World.__table__


class Fortune(Base):
    __tablename__ = 'fortune'
    id: Mapped[int] = mapped_column(primary_key=True)
    message: Mapped[str]

sa_fortunes = Fortune.__table__
