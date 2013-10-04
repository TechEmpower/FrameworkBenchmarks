from django.db import models

# Create your models here.

class World(models.Model):
  randomnumber = models.IntegerField()
  class Meta:
    db_table = 'World'

class Fortune(models.Model):
  message = models.CharField(max_length=65535)
  class Meta:
    db_table = 'Fortune'
