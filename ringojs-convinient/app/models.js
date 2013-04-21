var {Store, ConnectionPool, Cache} = require('ringo-sqlstore');

// DO NOT TOUCH THE FOLLOWING LINE.
// THIS VARIABLE IS REGEX REPLACED BY setup.py
var dbHost = 'localhost';

// create and configure store
var connectionPool = module.singleton("connectionPool", function() {
    return new ConnectionPool({
        "url": "jdbc:mysql://" + dbHost + "/hello_world",
        "driver": "com.mysql.jdbc.Driver",
        "username": "benchmarkdbuser",
        "password": "benchmarkdbpass"
    });
});
var store = exports.store = new Store(connectionPool);
var entityCache = module.singleton("entityCache", function() {
    return new Cache(10000);
});
var queryCache = module.singleton("queryCache", function() {
    return new Cache(10000);
});
store.setEntityCache(entityCache);
store.setQueryCache(queryCache);

// define entities in DB
exports.World = store.defineEntity('World', {
	table: 'World',
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