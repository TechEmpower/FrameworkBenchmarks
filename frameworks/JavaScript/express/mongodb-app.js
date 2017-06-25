
/**
 * Module dependencies.
 */

const cluster = require('cluster'),
  numCPUs = require('os').cpus().length,
  express = require('express'),
  mongoose = require('mongoose'),
  conn = mongoose.connect('mongodb://TFB-database/hello_world'),
  async = require('async');

// Middleware
const bodyParser = require('body-parser'),
  methodOverride = require('method-override'),
  errorHandler = require('errorhandler');

const Schema = mongoose.Schema,
  ObjectId = Schema.ObjectId;

const WorldSchema = new mongoose.Schema({
    id          : Number,
    randomNumber: Number
  }, {
    collection: 'world'
  }),
  MWorld = conn.model('world', WorldSchema);

const FortuneSchema = new mongoose.Schema({
    id          : Number,
    message     : String
  }, {
    collection: 'fortune'
  }),
  MFortune = conn.model('fortune', FortuneSchema);

if (cluster.isMaster) {
  // Fork workers.
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', (worker, code, signal) =>
    console.log('worker ' + worker.pid + ' died'));
} else {
  const app = module.exports = express();

  // Configuration
  // https://github.com/expressjs/method-override#custom-logic
  app.use(bodyParser.urlencoded({ extended: true }));
  app.use(methodOverride((req, res) => {
    if (req.body && typeof req.body === 'object' && '_method' in req.body) {
      // look in urlencoded POST bodies and delete it
      const method = req.body._method;
      delete req.body._method;
      return method;
    }
  }));

  // Set headers for all routes
  app.use((req, res, next) => {
    res.setHeader("Server", "Express");
    return next();
  });

  app.set('view engine', 'jade');
  app.set('views', __dirname + '/views');

  // Check Node env.
  const env = process.env.NODE_ENV || 'development';
  if ('development' == env) {
    app.use(errorHandler({ dumpExceptions: true, showStack: true }));
  }
  if ('production' == env) {
    app.use(errorHandler());
  }

  // Routes
  app.get('/mongoose', (req, res) => {
    let queriesRaw = parseInt(req.query.queries, 10),
      queries = isNaN(queriesRaw) ? 1 : queriesRaw;
    const queryFunctions = [];

    queries = Math.min(Math.max(queries, 1), 500);

    for (let i = 1; i <= queries; i++ ) {
      queryFunctions.push((callback) =>
        MWorld.findOne({ id: (Math.floor(Math.random() * 10000) + 1) })
          .exec(callback));
    }

    async.parallel(queryFunctions, (err, results) =>
      res.send(!req.query.queries ? results[0] : results));
  });

  app.get('/mongoose-fortune', (req, res) => {
    MFortune.find({}, (err, fortunes) => {
      const newFortune = {id: 0, message: "Additional fortune added at request time."};
      fortunes.push(newFortune);
      fortunes.sort((a, b) => (a.message < b.message) ? -1 : 1);

      res.render('fortunes', {fortunes: fortunes});
    });
  });

  app.get('/mongoose-update', (req, res) => {
    const selectFunctions = [],
        queries = Math.min(parseInt(req.query.queries) || 1, 500);

    for (let i = 1; i <= queries; i++ ) {
      selectFunctions.push((callback) =>
        MWorld.findOne({ id: Math.floor(Math.random() * 10000) + 1 })
          .exec(callback));
    }

    async.parallel(selectFunctions, (err, worlds) => {
      const updateFunctions = [];

      for (let i = 0; i < queries; i++) {
        ((i) => {
          updateFunctions.push((callback) => {
            worlds[i].randomNumber = Math.ceil(Math.random() * 10000);
            MWorld.update({
              id: worlds[i].id
            }, {
              randomNumber: worlds[i].randomNumber
            }, callback);
          });
        })(i);
      }

      async.parallel(updateFunctions, (err, updates) => res.send(worlds));
    });
  });

  app.listen(8080);
}
