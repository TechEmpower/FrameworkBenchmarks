from django.db import models

import os
MYSQL = os.environ['DJANGO_DB'] == 'mysql'

# Create your models here.

class World(models.Model):
  randomnumber = models.IntegerField()
  class Meta:
    db_table = 'World' if MYSQL else 'world'

class Fortune(models.Model):
  message = models.CharField(max_length=65535)
  class Meta:
    db_table = 'Fortune' if MYSQL else 'fortune'
