/**
 * Currently commenting out redis caching as there is no 
 * working implementation for the benchmark suite.
 */

const Hapi = require('hapi');
const Vision = require('vision');

const options = {
  connections: {
    compression: false
  }
};

const server = new Hapi.Server(options);

server.connection({port: 8080, host: '0.0.0.0'});
server.register(Vision, (err) => {
    if (err) {
        throw err;
    }

    server.views({
        engines: { html: require('handlebars') },
        path: __dirname + '/views/'
    });
});

const MongooseHandler = require('./handlers/mongoose');
const SequelizeHandler = require('./handlers/sequelize');
const SequelizePgHandler = require('./handlers/sequelize-postgres');

// Makes routing simpler as tfb routes are all GET's
// We also don't use the nifty route features that Hapi has
// to offer such as attaching a validator
const Route = (path, handler) =>
  server.route({ method: 'GET', path, handler });

const JsonSerialization = (req, reply) =>
  reply({ message: 'Hello, World!' }).header('Server', 'hapi');

const Plaintext = (req, reply) =>
  reply('Hello, World!')
    .header('Server', 'hapi')
    .header('Content-Type', 'text/plain');


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

server.start((err) =>
  console.log('Hapi worker started and listening on ' + server.info.uri + " "
    + new Date().toISOString(" ")));
