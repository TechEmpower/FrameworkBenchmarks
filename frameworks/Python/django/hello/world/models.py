from django.db import models


class World(models.Model):
    randomnumber = models.IntegerField()

    class Meta:
        db_table = 'world'


class Fortune(models.Model):
    message = models.CharField(max_length=65535)

    class Meta:
        db_table = 'fortune'
