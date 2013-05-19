/**
 * Module dependencies.
 */

var cluster = require('cluster'),
	numCPUs = require('os').cpus().length,
	windows = require('os').platform() == 'win32',
	Hapi = require('hapi'),
	mongoose = require('mongoose'),
	async = require('async'),
	conn = mongoose.connect('mongodb://localhost/hello_world'),
	connMap = { user: 'benchmarkdbuser', password: 'benchmarkdbpass', database: 'hello_world', host: 'localhost' };

var WorldSchema = new mongoose.Schema({
		id          : Number,
		randomNumber: Number
	}, {
		collection: 'world'
	}),
	MWorld = conn.model('World', WorldSchema);

if (!windows) {
	var Mapper = require('mapper');
	Mapper.connect(connMap, {verbose: false, strict: false});
	var World = Mapper.map('World', 'id', 'randomNumber');
	var Fortune = Mapper.map('Fortune', 'id', 'message');
}

if (cluster.isMaster) {
	// Fork workers.
	for (var i = 0; i < numCPUs; i++) {
		cluster.fork();
	}

	cluster.on('exit', function(worker, code, signal) {
		console.log('worker ' + worker.pid + ' died');
	});
} else {
	var server = module.exports = Hapi.createServer(null, 8080);

	server.route({
		method: 'GET',
		path: '/json',
		handler: function(req) {
			req.reply({ message: 'Hello World!' })
		}
	});

	server.route({
		method: 'GET',
		path: '/mongoose/{queries?}',
		handler: function(req){
			var queries = req.params.queries || 1,
				queryFunctions = [];

			for (var i = 1; i <= queries; i++) {
				queryFunctions.push(function(callback){
					MWorld.findOne({ id: (Math.floor(Math.random() * 10000) + 1) }).exec(callback);
				});
			}

			async.parallel(queryFunctions, function(err, results){
				req.reply(results);
			});
		}
	});

	server.route({
		method: 'GET',
		path: '/mysql-orm/{queries?}',
		handler: function(req){
			if (windows) return req.reply(Hapi.error.internal('Not supported on windows'));

			var queries = req.params.queries || 1,
					queryFunctions = [];

			for (var i = 1; i <= queries; i++) {
				queryFunctions.push(function(callback){
					World.findById(Math.floor(Math.random() * 10000) + 1, callback);
				});
			}

			async.parallel(queryFunctions, function(err, results){
				req.reply(results);
			});
		}
	});

	server.start();
}
