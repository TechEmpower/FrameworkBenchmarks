from random import randint

print "USE tfb;"

for i in range(1, 10001):
  print "INSERT INTO world (id, randomnumber) VALUES (%d, %d);" % (i, randint(1, 10000))
