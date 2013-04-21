use hello_world
db.world.drop()
for (var i = 1; i <= 10000; i++) {
  db.world.save( { id: i, randomNumber: (Math.floor(Math.random() * 10000) + 1) })
}

// http://docs.mongodb.org/manual/applications/optimization/
db.world.ensureIndex({id: 1})

db.fortune.drop()

db.fortune.save( {id: 1, message: 'fortune: No such file or directory'} );
db.fortune.save( {id: 2, message: "A computer scientist is someone who fixes things that aren't broken."} );
db.fortune.save( {id: 3, message: 'After enough decimal places, nobody gives a damn.'} );
db.fortune.save( {id: 4, message: 'A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1'} );
db.fortune.save( {id: 5, message: 'A computer program does what you tell it to do, not what you want it to do.'} );
db.fortune.save( {id: 6, message: 'Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen'} );
db.fortune.save( {id: 7, message: 'Any program that runs right is obsolete.'} );
db.fortune.save( {id: 8, message: 'A list is only as strong as its weakest link. — Donald Knuth'} );
db.fortune.save( {id: 9, message: 'Feature: A bug with seniority.'} );
db.fortune.save( {id: 10, message: 'Computers make very fast, very accurate mistakes.'} );
db.fortune.save( {id: 11, message: '<script>alert("This should not be displayed in a browser alert box.");</script>'} );
db.fortune.save( {id: 12, message: 'フレームワークのベンチマーク'} );

db.fortune.ensureIndex({id: 1})