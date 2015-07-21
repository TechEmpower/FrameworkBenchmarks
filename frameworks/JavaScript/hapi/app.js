/**
 * Module dependencies.
 */

var cluster = require('cluster'),
	numCPUs = require('os').cpus().length,
	Hapi = require('hapi'),
	Sequelize = require('sequelize'),
	mongoose = require('mongoose'),
	conn = mongoose.connect('mongodb://localhost/hello_world'),
	async = require('async');

var WorldSchema = new mongoose.Schema({
		id          : Number,
		randomNumber: Number
	}, {
		collection: 'world'
	}),
	MWorld = conn.model('World', WorldSchema);

var sequelize = new Sequelize('hello_world', 'benchmarkdbuser', 'benchmarkdbpass', {
	host: 'localhost',
	dialect: 'mysql',
	logging: false
});

var World = sequelize.define('World', {
	id: {
		type: 'Sequelize.INTEGER'
	},
	randomNumber: {
		type: 'Sequelize.INTEGER'
	}
}, {
	timestamps: false,
	freezeTableName: true
});
var Fortune = sequelize.define('Fortune', {
	id: {
		type: 'Sequelize.INTEGER'
	},
	message: {
		type: 'Sequelize.STRING'
	}
}, {
	timestamps: false,
	freezeTableName: true
});

if (cluster.isMaster) {
	// Fork workers.
	for (var i = 0; i < numCPUs; i++) {
		cluster.fork();
	}

	cluster.on('exit', function(worker, code, signal) {
		console.log('worker ' + worker.pid + ' died');
	});
} else {
	var server = module.exports = new Hapi.Server();
	server.connection({port: 8080});
	server.views({
		engines: {
			html: require('handlebars')
		},
		path: __dirname + '/views'
	});

	server.route({
		method: 'GET',
		path: '/json',
		handler: function(req, reply) {
			reply({ message: 'Hello, World!' }).header('Server', 'hapi');
		}
	});

	server.route({
		method: 'GET',
		path: '/plaintext',
		handler: function(req, reply) {
			reply('Hello, World!')
			 .header('Server', 'hapi')
			 .header('Content-Type', 'text/plain');
		}
	});

	server.route({
		method: 'GET',
		path: '/mongoose/{queries?}',
		handler: function(req, reply){
			var queries = isNaN(req.params.queries) ? 1 : parseInt(req.params.queries, 10),
				queryFunctions = [];

			queries = Math.min(Math.max(queries, 1), 500);

			for (var i = 1; i <= queries; i++) {
				queryFunctions.push(function(callback){
					MWorld.findOne({ id: (Math.floor(Math.random() * 10000) + 1) }).exec(callback);
				});
			}

			async.parallel(queryFunctions, function(err, results){
				if (!req.params.queries) {
					results = results[0];
				}
				reply(results).header('Server', 'hapi');
			});
		}
	});

	server.route({
		method: 'GET',
		path: '/mysql-orm/{queries?}',
		handler: function(req, reply){
			var queries = isNaN(req.params.queries) ? 1 : parseInt(req.params.queries, 10),
				queryFunctions = [];

			queries = Math.min(Math.max(queries, 1), 500);

			for (var i = 1; i <= queries; i++) {
				queryFunctions.push(function(callback){
					World.findOne({
						where: {
							id: Math.floor(Math.random() * 10000) + 1}
						}
					).complete(callback);
				});
			}

			async.parallel(queryFunctions, function(err, results){
				if (!req.params.queries) {
					results = results[0];
				}
				reply(results).header('Server', 'hapi');
			});
		}
	});

	server.route({
		method: 'GET',
		path: '/fortune',
		handler: function(req,reply){
			Fortune.findAll().complete(function(err, fortunes){
				fortunes.push({
					id: 0,
					message: 'Additional fortune added at request time.'
				});
				fortunes.sort(function(a, b){
					return (a.message < b.message) ? -1 : 1;
				});

				reply.view('fortunes', {
					fortunes: fortunes
				}).header('Server', 'hapi');
			});
		}
	});

	server.route({
		method: 'GET',
		path: '/mongoose-update/{queries?}',
		handler: function(req, reply){
			var queries = isNaN(req.params.queries) ? 1 : parseInt(req.params.queries, 10),
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
					reply(worlds).header('Server', 'hapi');
				});
			});
		}		
	});

	server.route({
		method: 'GET',
		path: '/mysql-orm-update/{queries?}',
		handler: function(req,reply){
			var queries = isNaN(req.params.queries) ? 1 : parseInt(req.params.queries, 10),
				selectFunctions = [];

			queries = Math.max(Math.min(queries, 500), 1);

			for (var i = 1; i <= queries; i++) {
				selectFunctions.push(function(callback){
					World.findOne({
						where: {
							id: Math.floor(Math.random() * 10000) + 1}
						}
					).complete(callback);
				});
			}

			async.parallel(selectFunctions, function(err, worlds) {
				var updateFunctions = [];

				for (var i = 0; i < queries; i++) {
					(function(i){
						updateFunctions.push(function(callback){
							worlds[i].randomNumber = Math.ceil(Math.random() * 10000);
							worlds[i].save().complete(callback);
						});
					})(i);
				}

				async.parallel(updateFunctions, function(err, updates) {
					reply(worlds).header('Server', 'hapi');
				});
			});
		}
	});

	server.start();
}
