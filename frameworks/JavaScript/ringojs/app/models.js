const { Store, Cache } = require('ringo-sqlstore');

// create and configure store
const connectionPool = module.singleton("connectionPool", function () {
    const mysqlConnectionProperties = "?jdbcCompliantTruncation=false&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useServerPrepStmts&enableQueryTimeouts=false&useUnbufferedIO=false&useReadAheadInput=false&maintainTimeStats=false&cacheRSMetadata=true";
    return Store.initConnectionPool({
        "url": "jdbc:mysql://TFB-database/hello_world" + mysqlConnectionProperties,
        "driver": "com.mysql.jdbc.Driver",
        "username": "benchmarkdbuser",
        "password": "benchmarkdbpass",
        "minimumIdle": 10,
        "maximumPoolSize": 30
    });
});
const store = exports.store = new Store(connectionPool);

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

const Fortune = exports.Fortune = store.defineEntity('Fortune', {
	table: 'Fortune',
	properties: {
		message: 'string'
	}
});

Fortune.sort = function(a, b) {
  return (a.message < b.message) ? -1 : (a.message > b.message) ? 1 : 0;
};
