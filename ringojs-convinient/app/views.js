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
   var helloObject = {message: "Hello, world"};
   return response.json(helloObject);
});

app.get('/db/:queries?', function(request, queries) {
   queries = parseInt(queries, 10) || 1;
   var worlds = [];
   var randId, world;
   for (var i = 0; i < queries; i++) {
      randId = ((Math.random() * 10000) | 0) + 1;
      world = models.store.query('select World.* from World where World.id = :id', {id: randId})[0];
      worlds.push(world.toJSON());
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
