var Hapi = require('hapi');
var server = new Hapi.Server();
server.connection({port: 8080});
server.views({
  engines: {
    hbs: require('handlebars')
  },
  path: __dirname + '/views',
  compileOptions: {
    pretty: false
  }
});

var Promise = require('bluebird');
var MongooseHandler;
var SequelizeHandler;
var SequelizePgHandler;
var RedisHandler;

// Slight start-up improvement loading handlers in parallel
Promise.join(
  require('./handlers/mongoose'),
  require('./handlers/sequelize'),
  require('./handlers/sequelize-postgres'),
  require('./handlers/redis'),
  function (mongo, mysql, pg, redis) {
    MongooseHandler = mongo;
    SequelizeHandler = mysql;
    SequelizePgHandler = pg;
    RedisHandler = redis;
  })
  .catch(function (err) {
    console.log('There was a problem setting up the handlers');
    process.exit(1);
  });


Route('/json', JsonSerialization);
Route('/plaintext', Plaintext);

Route('/mongoose/db', MongooseHandler.SingleQuery);
Route('/mongoose/queries', MongooseHandler.MultipleQueries);
Route('/mongoose/fortunes', MongooseHandler.Fortunes);
Route('/mongoose/updates', MongooseHandler.Updates);

Route('/sequelize/db', SequelizeHandler.SingleQuery);
Route('/sequelize/queries', SequelizeHandler.MultipleQueries);
Route('/sequelize/fortunes', SequelizeHandler.Fortunes);
Route('/sequelize/updates', SequelizeHandler.Updates);

Route('/sequelize-pg/db', SequelizePgHandler.SingleQuery);
Route('/sequelize-pg/queries', SequelizePgHandler.MultipleQueries);
Route('/sequelize-pg/fortunes', SequelizePgHandler.Fortunes);
Route('/sequelize-pg/updates', SequelizePgHandler.Updates);

Route('/hiredis/db', RedisHandler.SingleQuery);
Route('/hiredis/queries', RedisHandler.MultipleQueries);
Route('/hiredis/fortunes', RedisHandler.Fortunes);
Route('/hiredis/updates', RedisHandler.Updates);


function JsonSerialization(req, reply) {
  reply({ message: 'Hello, World!' })
    .header('Server', 'hapi');
}

function Plaintext(req, reply) {
  reply('Hello, World!')
    .header('Server', 'hapi')
    .header('Content-Type', 'text/plain');
}

// Makes routing simpler as tfb routes are all GET's
// We also don't use the nifty route features that Hapi has
// to offer such as attaching a validator
function Route(path, handler) {
  server.route({ method: 'GET', path: path, handler: handler})
}

server.start(function (err) {
  console.log('Hapi worker started and listening on ' + server.info.uri + " "
    + new Date().toISOString(" "));
});