from pony.orm import Database, Optional

db = Database()


class Json():
    pass


class World(db.Entity):
    randomnumber = Optional(int)


class WorldQueries():
    def __init__(self, queries):
        self.queries = queries


class Fortune(db.Entity):
    message = Optional(str)


class WorldUpdates():
    def __init__(self, queries):
        self.queries = queries


class Plaintext():
    pass
