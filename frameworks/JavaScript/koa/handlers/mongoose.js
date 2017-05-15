// Connects to MongoDB using the mongoose driver
// Handles related routes

const Promise = require('bluebird');
const h = require('../helper');
// Can treat mongoose library as one that supports Promises
// these methods will then have "-Async" appended to them.
const Mongoose = Promise.promisifyAll(require('mongoose'));
const connection = Mongoose.connect('mongodb://TFB-database/hello_world');

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

  SingleQuery: (ctx, next) => {
    return randomWorldPromise()
      .then((world) => {
        ctx.set('Server', 'Koa');
        ctx.type = 'application/json';
        ctx.body = world;
        return next();
      });
  },

  MultipleQueries: (ctx, next) => {
    const queries = h.getQueries(ctx);
    const worldPromises = h.fillArray(randomWorldPromise(), queries);

    return Promise
      .all(worldPromises)
      .then((worlds) => {
        ctx.set('Server', 'Koa');
        ctx.type = 'application/json';
        ctx.body = worlds;
        return next()
      });
  },

  Fortunes: (ctx, next) => {
    return promiseAllFortunes()
      .then((fortunes) => {
        fortunes.push(h.additionalFortune());
        fortunes.sort((a, b) => a.message.localeCompare(b.message));
      
        ctx.type = 'text/html';
        ctx.set('Server', 'Koa');
        return ctx.render('fortunes', { fortunes: fortunes });
      });
  },

  Updates: (ctx, next) => {
    const queries = h.getQueries(ctx);
    const worldPromises = [];

    for (let i = 0; i < queries; i++) {
      worldPromises.push(randomWorldPromise());
    }

    return Promise
      .all(worldPromises)
      .map((world) => {
        world.randomNumber = h.randomTfbNumber();
        return updateWorld(world);
      })
      .then((worlds) => {
        ctx.set('Server', 'Koa');
        ctx.type = 'application/json';
        ctx.body = worlds;
        return next();
      });
  }

};
