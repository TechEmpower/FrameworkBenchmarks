use hello_world
db.world.drop()
for (var i = 1; i <= 10000; i++) {
  db.world.save( { id: i, randomNumber: (Math.floor(Math.random() * 10000) + 1) })
}

// http://docs.mongodb.org/manual/applications/optimization/
db.world.ensureIndex({id: 1})