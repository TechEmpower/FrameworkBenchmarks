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

var MongooseHandler = require('./handlers/mongoose');
var SequelizeHandler = require('./handlers/sequelize');

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
  console.log('Hapi worker started and listening on ' + server.info.uri);
});