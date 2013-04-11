from django.db import models

# Create your models here.

class World(models.Model):
  randomNumber = models.IntegerField()
  class Meta:
    db_table = 'World'

