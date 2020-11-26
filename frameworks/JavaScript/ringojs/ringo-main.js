const sql = require('sql-ringojs-client');
const fs = require('fs');

exports.app = function (req) {
  const path = req.pathInfo;
  let connection, body;

  if (path === '/json') {
    const helloObject = { message: "Hello, World!" };
    // JSON Response Test
    return {
      status: 200,
      headers: { "Content-Type": "application/json" },
      body: [JSON.stringify(helloObject)]
    }
  }

  if (path === '/db') {
    try {
      connection = datasource.getConnection();
      let randId, world;
      randId = ((Math.random() * 10000) | 0) + 1;
      world = sql.query(connection, 'select * from World where World.id = ' + randId)[0];
      return {
        status: 200,
        headers: { "Content-Type": "application/json" },
        body: [JSON.stringify(world)]
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

  if (path === '/dbquery') {
    let queryCount = req.env.servletRequest.getParameter('queries');
    try {
      connection = datasource.getConnection();
      let randId, world;
      if (!queryCount || isNaN(queryCount) || queryCount < 1) {
        queryCount = 1;
      } else {
        queryCount = Math.min(Math.max(parseInt(queryCount, 10) || 1, 1), 500);
      }
      body = [];
      for (let i = 0; i < queryCount; i++) {
        randId = ((Math.random() * 10000) | 0) + 1;
        world = sql.query(connection, 'select * from World where World.id = ' + randId)[0];
        body.push(world);
      }
      return {
        status: 200,
        headers: { "Content-Type": "application/json" },
        body: [JSON.stringify(body)]
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

  if (path === '/plaintext') {
    return {
      status: 200,
      headers: { "Content-Type": 'text/plain' },
      body: ['Hello, World!']
    };
  }

  if (path === '/updates') {
    let queryCount = parseInt(req.env.servletRequest.getParameter('queries'), 10);
    if (isNaN(queryCount) || queryCount < 1) {
      queryCount = 1;
    } else if (queryCount > 500) {
      queryCount = 500;
    }
    try {
      connection = datasource.getConnection();
      body = [];
      for (let i = 0; i < queryCount; i++) {
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
      headers: { "Content-Type": "application/json; charset=UTF-8" },
      body: [JSON.stringify(body)]
    }
  }
};


const datasource = module.singleton('pooling-datasource', function () {
  const mysqlConnectionProperties = "?jdbcCompliantTruncation=false&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useServerPrepStmts=true&enableQueryTimeouts=false&useUnbufferedIO=false&useReadAheadInput=false&maintainTimeStats=false&cacheRSMetadata=true&useSSL=false";
  return sql.connect("jdbc:mysql://tfb-database/hello_world" + mysqlConnectionProperties, 'benchmarkdbuser', 'benchmarkdbpass');
});

if (require.main == module) {
  require("ringo/httpserver").main(module.id);
}
