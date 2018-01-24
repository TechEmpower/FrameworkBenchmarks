// Connects to MongoDB using the mongoose driver
// Handles related routes

const Promise = require('bluebird');
const h = require('../helper');
// Can treat mongoose library as one that supports Promises
// these methods will then have "-Async" appended to them.
const Mongoose = Promise.promisifyAll(require('mongoose'));
const connection = Mongoose.connect(
  'mongodb://TFB-database/hello_world',
  { useMongoClient: true }
);

const WorldSchema = new Mongoose.Schema({
    id :          Number,
    randomNumber: Number
  }, {
    collection: 'world'
  });
const FortuneSchema = new Mongoose.Schema({
    id:      Number,
    message: String
  }, {
    collection: 'fortune'
  });

const Worlds = connection.model('World', WorldSchema);
const Fortunes = connection.model('Fortune', FortuneSchema);

const randomWorldPromise = () =>
  Worlds.findOneAsync({ id: h.randomTfbNumber() })
    .then((world) => world)
    .catch((err) => process.exit(1));


const promiseAllFortunes = () =>
  Fortunes.findAsync({})
    .then((fortunes) => fortunes)
    .catch((err) => process.exit(1));

const updateWorld = (world) =>
  Worlds
    .updateAsync(
      { id: world.randomNumber },
      { randomNumber: world.randomNumber }
    )
    .then((result) => world)
    .catch((err) => process.exit(1));

module.exports = {

  SingleQuery: (req, reply) => {
    randomWorldPromise()
      .then((world) => {
        reply(world)
          .header('Server', 'hapi');
      });
  },

  MultipleQueries: (req, reply) => {
    const queries = h.getQueries(req);
    const worldPromises = h.fillArray(randomWorldPromise(), queries);

    Promise
      .all(worldPromises)
      .then((worlds) => {
        reply(worlds)
          .header('Server', 'hapi');
      });
  },

  Fortunes: (req, reply) => {
    promiseAllFortunes()
      .then((fortunes) => {
        fortunes.push(h.additionalFortune());
        fortunes.sort((a, b) => a.message.localeCompare(b.message));
      
        reply.view('fortunes', {
          fortunes: fortunes
        })
          .header('Content-Type', 'text/html')
          .header('Server', 'hapi');
      });
  },

  Updates: (req, reply) => {
    const queries = h.getQueries(req);
    const worldPromises = [];

    for (let i = 0; i < queries; i++) {
      worldPromises.push(randomWorldPromise());
    }

    Promise
      .all(worldPromises)
      .map((world) => {
        world.randomNumber = h.randomTfbNumber();
        return updateWorld(world);
      })
      .then((worlds) => {
        reply(worlds)
          .header('Server', 'hapi');
      });
  }

};
