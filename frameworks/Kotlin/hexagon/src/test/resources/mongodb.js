
db = db.getSiblingDB('hello_world');

db.world.drop();

for (var i = 1; i <= 10000; i++) {
  db.world.save( { _id: i, id: i, randomNumber: (Math.floor(Math.random() * 10000) + 1) });
}

db.world.ensureIndex({_id: 1});

function saveFortune(id, message) {
  db.fortune.save( { _id: id, id: id, message: message } );
}

db.fortune.drop();

saveFortune(1, 'fortune: No such file or directory');
saveFortune(2, "A computer scientist is someone who fixes things that aren't broken.");
saveFortune(3, 'After enough decimal places, nobody gives a damn.');
saveFortune(4, 'A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1');
saveFortune(5, 'A computer program does what you tell it to do, not what you want it to do.');
saveFortune(6, 'Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen');
saveFortune(7, 'Any program that runs right is obsolete.');
saveFortune(8, 'A list is only as strong as its weakest link. — Donald Knuth');
saveFortune(9, 'Feature: A bug with seniority.');
saveFortune(10, 'Computers make very fast, very accurate mistakes.');
saveFortune(11, '<script>alert("This should not be displayed in a browser alert box.");</script>');
saveFortune(12, 'フレームワークのベンチマーク');

db.fortune.ensureIndex({_id: 1});
