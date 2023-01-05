const {Application} = require("stick");
const fs = require('fs');
const response = require('ringo/jsgi/response');
const models = require('./models');

const { Template } = require('reinhardt/template');
const fortuneTemplate = module.singleton('fortuneTemplate', function() {
   return new Template(fs.read(module.resolve('../templates/fortunes.reinhardt')));
});

const app = exports.app = Application();

const formatQueries = function(queries) {
  queries = parseInt(queries, 10) || 1;
  if (isNaN(queries) || queries < 1) {
    queries = 1;
  } else if (queries > 500) {
    queries = 500;
  }
  return queries;
};

app.configure("params", "route");

app.get('/json', function() {
   const helloObject = {message: "Hello, World!"};
   return response.json(helloObject);
});

app.get('/db', function() {
      let randId = ((Math.random() * 10000) | 0) + 1;
      let world = models.store.query('select * from World where id = :id', {id: randId})[0];
   return response.json(world);
});

app.get('/dbquery/:queries?', function(request, queries) {
   queries = formatQueries(queries);
   let worlds = [];
   for (let i = 0; i < queries; i++) {
      let randId = ((Math.random() * 10000) | 0) + 1;
      let world = models.store.query('select * from World where id = :id', {id: randId})[0];
      worlds.push({"id": world.id, "randomNumber" : world.randomNumber});
   }
   return response.json(worlds);
});

app.get('/fortunes', function() {
   const fortunes = models.store.query('select Fortune.* from Fortune');
   fortunes.push({
      id: 0,
      message: 'Additional fortune added at request time.'
   });
   fortunes.sort(models.Fortune.sort);
   return response.html(fortuneTemplate.render({fortunes: fortunes}));
});

app.get('/plaintext', function() {
   return response.text('Hello, World!');
});

app.get('/updates/:queries?', function(request, queries) {
   queries = formatQueries(queries);
   const worlds = [];
   for (let i = 0; i < queries; i++) {
      let randId = ((Math.random() * 10000) | 0) + 1;
      let world = models.store.query('select * from World where id = :id', {id: randId})[0];
      world.randomNumber = ((Math.random() * 10000) | 0) + 1;
      try {
         world.save();
      } catch (e) {
         return response.error('SQL error');
      }
      worlds.push({"id": world.id, "randomNumber": world.randomNumber});
   }
   return response.json(worlds);
});
