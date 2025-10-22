from tortoise.models import Model
from tortoise import fields


class World(Model):
    class Meta:
        table = "world"

    id = fields.IntField(pk=True)
    randomNumber = fields.IntField(source_field="randomnumber")

    def to_dict(self):
        """Return object data in easily serializeable format"""
        return {"id": self.id, "randomNumber": self.randomNumber}


class Fortune(Model):
    class Meta:
        table = "fortune"

    id = fields.IntField(pk=True)
    message = fields.TextField()


__models__ = [World, Fortune]
