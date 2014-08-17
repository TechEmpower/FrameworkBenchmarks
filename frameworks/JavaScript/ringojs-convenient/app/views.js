var {Application} = require("stick");
var fs = require('fs');
var response = require('ringo/jsgi/response');
var models = require('./models');

var {Template} = require('reinhardt/template');
var fortuneTemplate = module.singleton('fortuneTemplate', function() {
   return new Template(fs.read(module.resolve('../templates/fortunes.reinhardt')));
});

var app = exports.app = Application();
app.configure("params", "route");

app.get('/json', function() {
   var helloObject = {message: "Hello, World!"};
   return response.json(helloObject);
});

app.get('/db/:queries?', function(request, queries) {
   queries = parseInt(queries, 10) || 1;
   var worlds = [];
   var randId, world;
   for (var i = 0; i < queries; i++) {
      randId = ((Math.random() * 10000) | 0) + 1;
      world = models.store.query('select World.* from World where World.id = :id', {id: randId})[0];
      worlds.push({"id": world._id, "randomNumber" : world.randomNumber});
   }
   if (queries == 1) {
      worlds = worlds[0];
   }
   return response.json(worlds);
});

app.get('/fortune', function() {
   var fortunes = models.store.query('select Fortune.* from Fortune');
   fortunes.push({
      _id: 0,
      message: 'Additional fortune added at request time.'
   });
   fortunes.sort(models.Fortune.sort);
   return response.html(fortuneTemplate.render({fortunes: fortunes}));
});

app.get('/plaintext', function() {
   return response.text('Hello, World!');
});

app.get('/updates/:queries?', function(request, queries) {
   queries = parseInt(queries, 10) || 1;
   if (isNaN(queries) || queries < 1) {
      queries = 1;
   } else if (queries > 500) {
      queries = 500;
   }
   var worlds = [];
   var randId, world;
   models.store.beginTransaction();
   for (var i = 0; i < queries; i++) {
      randId = ((Math.random() * 10000) | 0) + 1;
      world = models.store.query('select World.* from World where World.id = :id', {id: randId})[0];
      world.randomNumber = ((Math.random() * 10000) | 0) + 1;
      try {
         world.save();
      } catch (e) {
         models.store.abortTransaction();
         return response.error('SQL error');
      }
      worlds.push({"id": world._id, "randomNumber": world.randomNumber});
   }
   models.store.commitTransaction();
   return response.json(worlds);
});