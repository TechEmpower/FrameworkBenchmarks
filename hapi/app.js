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
	connMap = { user: 'benchmarkdbuser', password: 'benchmarkdbpass', database: 'hello_world', host: 'localhost', charset: 'utf8' };

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
	var server = module.exports = Hapi.createServer(null, 8080, {
		views: {
			engines: {
				handlebars: 'handlebars'
			},
			path: __dirname + '/views'
		}
	});

	server.route({
		method: 'GET',
		path: '/json',
		handler: function(req) {
			req.reply({ message: 'Hello, World!' })
		}
	});

	server.route({
		method: 'GET',
		path: '/mongoose/{queries?}',
		handler: function(req){
			var queries = req.params.queries || 1,
				queryFunctions = [];

			queries = Math.min(Math.max(queries, 1), 500);

			for (var i = 1; i <= queries; i++) {
				queryFunctions.push(function(callback){
					MWorld.findOne({ id: (Math.floor(Math.random() * 10000) + 1) }).exec(callback);
				});
			}

			async.parallel(queryFunctions, function(err, results){
				if (queries == 1) {
					results = results[0];
				}
				req.reply(results).header('Server', 'hapi');
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

			queries = Math.min(Math.max(queries, 1), 500);

			for (var i = 1; i <= queries; i++) {
				queryFunctions.push(function(callback){
					World.findById(Math.floor(Math.random() * 10000) + 1, callback);
				});
			}

			async.parallel(queryFunctions, function(err, results){
				if (queries == 1) {
					results = results[0];
				}
				req.reply(results).header('Server', 'hapi');
			});
		}
	});

	server.route({
		method: 'GET',
		path: '/fortune',
		handler: function(req){
			if (windows) return req.reply(Hapi.error.internal('Not supported on windows'));

			Fortune.all(function(err, fortunes){
				fortunes.push({
					id: 0,
					message: 'Additional fortune added at request time.'
				});
				fortunes.sort(function(a, b){
					return (a.message < b.message) ? -1 : 1;
				});

				req.reply.view('fortunes.handlebars', {
					fortunes: fortunes
				}).header('Server', 'hapi');
			});
		}
	});

	server.route({
		method: 'GET',
		path: '/mongoose-update/{queries?}',
		handler: function(req){
			var queries = req.params.queries || 1,
				selectFunctions = [];

			queries = Math.max(Math.min(queries, 500), 1);

			for (var i = 1; i <= queries; i++) {
				selectFunctions.push(function(callback){
					MWorld.findOne({ id: Math.floor(Math.random() * 10000) + 1 }).exec(callback);
				});
			}

			async.parallel(selectFunctions, function(err, worlds) {
				var updateFunctions = [];

				for (var i = 0; i < queries; i++) {
					(function(i){
						updateFunctions.push(function(callback){
							worlds[i].randomNumber = Math.ceil(Math.random() * 10000);
							MWorld.update({
								id: worlds[i]
							}, {
								randomNumber: worlds[i].randomNumber
							}, callback);
						});
					})(i);
				}

				async.parallel(updateFunctions, function(err, updates) {
					req.reply(worlds).header('Server', 'hapi');
				});
			});
		}		
	});

	server.route({
		method: 'GET',
		path: '/mysql-orm-update/{queries?}',
		handler: function(req){
			var queries = req.params.queries || 1,
				selectFunctions = [];

			queries = Math.max(Math.min(queries, 500), 1);

			for (var i = 1; i <= queries; i++) {
				selectFunctions.push(function(callback){
					World.findById(Math.floor(Math.random() * 10000) + 1, callback);
				});
			}

			async.parallel(selectFunctions, function(err, worlds) {
				var updateFunctions = [];

				for (var i = 0; i < queries; i++) {
					(function(i){
						updateFunctions.push(function(callback){
							worlds[i].randomNumber = Math.ceil(Math.random() * 10000);
							World.save(worlds[i], callback);
						});
					})(i);
				}

				async.parallel(updateFunctions, function(err, updates) {
					req.reply(worlds).header('Server', 'hapi');
				});
			});
		}
	});

	server.start();
}
