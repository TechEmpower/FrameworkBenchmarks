
/**
 * Module dependencies.
 */

const cluster = require('cluster'),
  numCPUs = require('os').cpus().length,
  express = require('express'),
  mongoose = require('mongoose'),
  conn = mongoose.connect('mongodb://tfb-database/hello_world');

// Middleware
const bodyParser = require('body-parser');

const Schema = mongoose.Schema,
  ObjectId = Schema.ObjectId;

const WorldSchema = new mongoose.Schema({
  id: Number,
  randomNumber: Number
}, {
    collection: 'world'
  }),
  MWorld = mongoose.model('world', WorldSchema);

const FortuneSchema = new mongoose.Schema({
  id: Number,
  message: String
}, {
    collection: 'fortune'
  }),
  MFortune = mongoose.model('fortune', FortuneSchema);

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
  app.use(bodyParser.urlencoded({ extended: true }));

  // Set headers for all routes
  app.use((req, res, next) => {
    res.setHeader("Server", "Express");
    return next();
  });

  app.set('view engine', 'pug');
  app.set('views', __dirname + '/views');

  // Routes
  app.get('/mongooseq', async (req, res) => {
    const queries = Math.min(parseInt(req.query.queries) || 1, 500),
      results = [];

    for (let i = 1; i <= queries; i++) {
      results.push(await MWorld.findOne({ id: (Math.floor(Math.random() * 10000) + 1) }));
    }

    res.send(results);
  });

  app.get('/mongoose', async (req, res) => {
    let results = await MWorld.findOne({ id: (Math.floor(Math.random() * 10000) + 1) });

    res.send(results);
  });

  app.get('/mongoose-fortune', (req, res) => {
    MFortune.find({}, (err, fortunes) => {
      const newFortune = { id: 0, message: "Additional fortune added at request time." };
      fortunes.push(newFortune);
      fortunes.sort((a, b) => (a.message < b.message) ? -1 : 1);

      res.render('fortunes/index', { fortunes: fortunes });
    });
  });

  app.get('/mongoose-update', async (req, res) => {
    const results = [],
      queries = Math.min(parseInt(req.query.queries) || 1, 500);

    for (let i = 1; i <= queries; i++) {
      const world = await MWorld.findOne({ id: (Math.floor(Math.random() * 10000) + 1) });
      world.randomNumber = ~~(Math.random() * 10000) + 1;
      await MWorld.update({
        id: world.id
      }, {
          randomNumber: world.randomNumber
        });

      results.push(world);
    }

    res.send(results);
  });

  app.listen(8080);
}
