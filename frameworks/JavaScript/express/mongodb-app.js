/**
 * Module dependencies.
 */

const cluster = require('cluster');
const numCPUs = require('os').cpus().length;
const express = require('express');
const mongoose = require('mongoose');
const conn = mongoose.connect('mongodb://tfb-database/hello_world');

// Middleware
const bodyParser = require('body-parser');

/**
 * Note! The benchmarks say we should use "id" as a property name.
 * However, Mongo provides a default index on "_id", so to be equivalent to the other tests, we use
 * the same, default index provided by the database.
 *
 */
const WorldSchema = new mongoose.Schema({
  _id: Number,
  randomNumber: Number
}, {
  collection: 'world'
});
const MWorld = mongoose.model('world', WorldSchema);

const FortuneSchema = new mongoose.Schema({
  _id: Number,
  message: String
}, {
  collection: 'fortune'
});
const MFortune = mongoose.model('fortune', FortuneSchema);

if (cluster.isPrimary) {
  // Fork workers.
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', (worker, code, signal) => console.log('worker ' + worker.pid + ' died'));
} else {
  const app = module.exports = express();

  const randomTfbNumber = () => Math.floor(Math.random() * 10000) + 1;
  const toClientWorld = (world) => {
    if (world) {
      world.id = world._id;
      delete world._id;
    }
    return world;
  };

  // Configuration
  app.use(bodyParser.urlencoded({extended: true}));

  // Set headers for all routes
  app.use((req, res, next) => {
    res.setHeader("Server", "Express");
    return next();
  });

  app.set('view engine', 'pug');
  app.set('views', __dirname + '/views');

  async function getRandomWorld() {
    return toClientWorld(await MWorld.findOne({_id: randomTfbNumber()}).lean().exec());
  }

  // Routes
  app.get('/mongooseq', async (req, res) => {
    const queryCount = Math.min(parseInt(req.query.queries) || 1, 500);
    const promises = [];

    for (let i = 1; i <= queryCount; i++) {
      promises.push(getRandomWorld());
    }

    res.send(await Promise.all(promises));
  });

  app.get('/mongoose', async (req, res) => {
    const result = await MWorld.findOne({_id: randomTfbNumber()}).lean().exec();

    res.send(toClientWorld(result));
  });

  app.get('/mongoose-fortune', async (req, res) => {
    const fortunes = (await MFortune.find({}).lean().exec()).map(toClientWorld);
    const newFortune = {id: 0, message: "Additional fortune added at request time."};
    fortunes.push(newFortune);
    fortunes.sort((a, b) => (a.message < b.message) ? -1 : 1);

    res.render('fortunes/index', {fortunes});
  });

  async function getUpdateRandomWorld() {
    // it would be nice to use findOneAndUpdate here, but for some reason the test fails with it.
    const world = await MWorld.findOne({_id: randomTfbNumber()}).lean().exec();
    world.randomNumber = randomTfbNumber();
    await MWorld.updateOne({
      _id: world._id
    }, {
      $set: {
        randomNumber: world.randomNumber
      }
    }).exec();
    return toClientWorld(world);
  }

  app.get('/mongoose-update', async (req, res) => {
    const queryCount = Math.min(parseInt(req.query.queries, 10) || 1, 500);
    const promises = [];

    for (let i = 1; i <= queryCount; i++) {
      promises.push(getUpdateRandomWorld());
    }

    res.send(await Promise.all(promises));
  });

  app.listen(8080);
}
