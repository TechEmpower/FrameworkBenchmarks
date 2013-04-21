var sql = require('sql-ringojs-client');
var {Application} = require("stick");
var fs = require('fs');
var {Template} = require('reinhardt/template');
var response = require('ringo/jsgi/response');

// DO NOT TOUCH THE FOLLOWING LINE.
// THIS VARIABLE IS REGEX REPLACED BY setup.py
var dbHost = 'localhost';

var sortFortunes = function(a, b) {
 return (a.message < b.message) ? -1 : (a.message > b.message) ? 1 : 0;
};

var fortuneTemplate = module.singleton('fortuneTemplate', function() {
   return new Template(fs.read(module.resolve('./templates/fortunes.reinhardt')));
})

var datasource = module.singleton('pooling-datasource', function() {
  return sql.connect("jdbc:mysql://" + dbHost + "/hello_world", 'benchmarkdbuser', 'benchmarkdbpass');
});

var app = exports.app = Application();
app.configure("params", "route");

app.get('/json', function() {
	var helloObject = {message: "Hello, world"};
	// JSON Response Test
	return {
      status: 200,
      headers: {"Content-Type": "application/json; charset=UTF-8"},
      body: [JSON.stringify(helloObject)]
	}
});

app.get('/db', function(request) {
   var queryCount = request.params.queries;
   try {
      var connection = datasource.getConnection();
      if (queryCount === null) {
         var randId = ((Math.random() * 10000) | 0) + 1
         var world = sql.query(connection, 'select * from World where World.id = ' + randId)[0];
         return response.json(world);
      } else {
         queryCount = parseInt(queryCount, 10);
         var worlds = [];
         var randId, world;
         for (var i = 0; i < queryCount; i++) {
            randId = ((Math.random() * 10000) | 0) + 1;
            world = sql.query(connection, 'select * from World where World.id = ' + randId)[0];
            worlds.push(world);
         }
         return response.json(worlds);
      }
   } catch (e) {
      connection.close();
      connection = null;
   } finally {
      if (connection !== null) {
         connection.close();
      }
   }
});

app.get('/fortune', function() {
   try {
      var connection = datasource.getConnection();
      var fortunes = sql.query(connection, 'select * from Fortune');
      fortunes.push({
         id: 0,
         message: 'Additional fortune added at request time.'
      });
      fortunes.sort(sortFortunes);
      return response.html(fortuneTemplate.render({fortunes: fortunes}));
   } catch (e) {
      connection.close();
      connection = null;
   } finally {
      if (connection !== null) {
         connection.close();
      }
   }
});
