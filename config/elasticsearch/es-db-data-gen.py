from random import randint

for i in range(1, 10001):
  print """{ "index" : { "_id" : "%s" } }
{ "randomNumber" : %s }""" % (i, randint(1, 10000))
