var sql = require('sql-ringojs-client');
var mustache = require('ringo/mustache');

// DO NOT TOUCH THE FOLLOWING LINE.
// THIS VARIABLE IS REGEX REPLACED BY setup.py
var dbHost = 'localhost';
var mongodbUri = 'mongodb://localhost/hello_world';

var sortFortunes = function(a, b) {
 return (a.message < b.message) ? -1 : (a.message > b.message) ? 1 : 0;
};

var fortuneTemplate = require('fs').read(module.resolve('./templates/fortune.mustache'));

exports.app = function(req) {
   var path = req.pathInfo;
   if (path === '/json') {
      var helloObject = {message: "Hello, world"};
      // JSON Response Test
      return {
         status: 200,
         headers: {"Content-Type": "application/json; charset=UTF-8"},
         body: [JSON.stringify(helloObject)]
      }
   } else if (path === '/db') {
      var queryCount = req.env.servletRequest.getParameter('queries');
      try {
         var connection = datasource.getConnection();
         if (queryCount === null) {
            var randId = ((Math.random() * 10000) | 0) + 1
            var world = sql.query(connection, 'select * from World where World.id = ' + randId)[0];
            return {
               status: 200,
               headers: {"Content-Type": "application/json; charset=UTF-8"},
               body: [JSON.stringify(world)]
            }
         } else {
            queryCount = parseInt(queryCount, 10);
            var body = [];
            var randId, world;
            for (var i = 0; i < queryCount; i++) {
               randId = ((Math.random() * 10000) | 0) + 1;
               world = sql.query(connection, 'select * from World where World.id = ' + randId)[0];
               body.push(world);
            }
            return {
               status: 200,
               headers: {"Content-Type": "application/json; charset=UTF-8"},
               body: [JSON.stringify(body)]
            }
         }
      } catch (e) {
         connection.close();
         connection = null;
      } finally {
         if (connection !== null) {
            connection.close();
         }
      }
   } else if (path === '/fortune') {
      try {
         var connection = datasource.getConnection();
         var fortunes = sql.query(connection, 'select * from Fortune');
         fortunes.push({
            id: 0,
            message: 'Additional fortune added at request time.'
         });
         fortunes.sort(sortFortunes);
         return {
            status: 200,
            headers: {"Content-Type": "text/html; charset=UTF-8"},
            body: [mustache.to_html(fortuneTemplate, {fortunes: fortunes})]
         }
      } catch (e) {
         connection.close();
         connection = null;
      } finally {
         if (connection !== null) {
            connection.close();
         }
      }
   }
};


var datasource = module.singleton('pooling-datasource', function() {
  return sql.connect("jdbc:mysql://" + dbHost + "/hello_world", 'benchmarkdbuser', 'benchmarkdbpass');
});

if (require.main == module) {
    require("ringo/httpserver").main(module.id);
}
