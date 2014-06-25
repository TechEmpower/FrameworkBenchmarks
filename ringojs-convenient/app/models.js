var {Store, ConnectionPool, Cache} = require('ringo-sqlstore');

// DO NOT TOUCH THE FOLLOWING LINE.
// THIS VARIABLE IS REGEX REPLACED BY setup.py
var dbHost = '172.16.98.98';

// create and configure store
var connectionPool = module.singleton("connectionPool", function() {
    var mysqlConnectionProperties = "?jdbcCompliantTruncation=false&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useServerPrepStmts&enableQueryTimeouts=false&useUnbufferedIO=false&useReadAheadInput=false&maintainTimeStats=false&cacheRSMetadata=true";
    return new ConnectionPool({
        "url": "jdbc:mysql://" + dbHost + "/hello_world" + mysqlConnectionProperties,
        "driver": "com.mysql.jdbc.Driver",
        "username": "benchmarkdbuser",
        "password": "benchmarkdbpass"
    });
});
var store = exports.store = new Store(connectionPool);
var queryCache = module.singleton("queryCache", function() {
    return new Cache(10000);
});
store.setQueryCache(queryCache);

// define entities in DB
exports.World = store.defineEntity('World', {
	table: 'World',
  id: {
    column: 'id'
  },
	properties: {
		randomNumber: 'integer'
	}
});

var Fortune = exports.Fortune = store.defineEntity('Fortune', {
	table: 'Fortune',
	properties: {
		message: 'string'
	}
});

Fortune.sort = function(a, b) {
 return (a.message < b.message) ? -1 : (a.message > b.message) ? 1 : 0;
};
