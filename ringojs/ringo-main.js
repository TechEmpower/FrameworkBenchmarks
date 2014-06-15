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
      var helloObject = {message: "Hello, World!"};
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
   } else if (path === '/plaintext') {
      return {
        status: 200,
        headers: {"Content-Type": 'text/plain'},
        body: ['Hello, World!']
      };
   } else if (path === '/updates') {
      var queryCount = parseInt(req.env.servletRequest.getParameter('queries'), 10);
      if (isNaN(queryCount) || queryCount < 1) {
         queryCount = 1;
      } else if (queryCount > 500) {
         queryCount = 500;
      }
      try {
         var connection = datasource.getConnection();
         var body = [];
         for (var i = 0; i < queryCount; i++) {
            let randId = ((Math.random() * 10000) | 0) + 1;
            world = sql.query(connection, 'select * from World where World.id = ' + randId)[0];
            world.randomNumber = ((Math.random() * 10000) | 0) + 1;
            sql.execute(connection, 'UPDATE World SET randomNumber = ' + world.randomNumber + ' WHERE id = ' + world.id);
            body.push(world);
         }
      } catch (e) {
         connection.close();
         connection = null;
      } finally {
         if (connection !== null) {
            connection.close();
         }
      }
      return {
         status: 200,
         headers: {"Content-Type": "application/json; charset=UTF-8"},
         body: [JSON.stringify(body)]
      }
   }
};


var datasource = module.singleton('pooling-datasource', function() {
  var mysqlConnectionProperties = "?jdbcCompliantTruncation=false&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useServerPrepStmts&enableQueryTimeouts=false&useUnbufferedIO=false&useReadAheadInput=false&maintainTimeStats=false&cacheRSMetadata=true";
  return sql.connect("jdbc:mysql://" + dbHost + "/hello_world" + mysqlConnectionProperties, 'benchmarkdbuser', 'benchmarkdbpass');
});

if (require.main == module) {
    require("ringo/httpserver").main(module.id);
}
