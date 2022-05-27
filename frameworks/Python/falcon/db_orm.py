import sys
import os
from pony.orm import (
    Database,
    PrimaryKey,
    Required,
    db_session as session
)


_is_pypy = hasattr(sys, "pypy_version_info")

if _is_pypy:
    from psycopg2cffi import compat

    compat.register()

db = Database()
db.bind(
    provider="postgres",
    host="tfb-database",
    port=5432,
    user=os.getenv("PGUSER", "benchmarkdbuser"),
    password=os.getenv("PSPASS", "benchmarkdbpass"),
    database="hello_world",
)


class World(db.Entity):
    _table_ = "world"
    id = PrimaryKey(int)
    randomNumber = Required(int, column="randomnumber")

    def to_dict(self):
        """Return object data in easily serializeable format"""
        return {"id": self.id, "randomNumber": self.randomNumber}


class Fortune(db.Entity):
    _table_ = "fortune"
    id = PrimaryKey(int, auto=True)
    message = Required(str)


db.generate_mapping(create_tables=False)
