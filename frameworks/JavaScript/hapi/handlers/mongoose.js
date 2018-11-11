// Connects to MongoDB using the mongoose driver
// Handles related routes

const h = require('../helper');
const Mongoose = require('mongoose');
Mongoose.connect('mongodb://tfb-database/hello_world');

const WorldSchema = new Mongoose.Schema({
  id: Number,
  randomNumber: Number
}, {
    collection: 'world'
  });

const FortuneSchema = new Mongoose.Schema({
  id: Number,
  message: String
}, {
    collection: 'fortune'
  });

const Worlds = Mongoose.model('world', WorldSchema);
const Fortunes = Mongoose.model('fortune', FortuneSchema);

const randomWorld = async () =>
  await Worlds.findOne({ id: h.randomTfbNumber() });

const updateWorld = async (world) =>
  await Worlds.update(
    { id: world.randomNumber },
    { randomNumber: world.randomNumber }
  );

module.exports = {

  SingleQuery: async (req, reply) => {
    reply(await randomWorld()).header('Server', 'hapi');
  },

  MultipleQueries: async (req, reply) => {
    const queries = h.getQueries(req);
    const results = h.fillArray(await randomWorld(), queries);

    reply(results).header('Server', 'hapi');
  },

  Fortunes: async (req, reply) => {
    const fortunes = await Fortunes.find();
    fortunes.push(h.additionalFortune());
    fortunes.sort((a, b) => a.message.localeCompare(b.message));

    reply.view('fortunes', {
      fortunes: fortunes
    })
      .header('Content-Type', 'text/html')
      .header('Server', 'hapi');
  },

  Updates: async (req, reply) => {
    const queries = h.getQueries(req);
    const results = [];

    for (let i = 0; i < queries; i++) {
      const world = await randomWorld();
      world.randomNumber = h.randomTfbNumber();
      await updateWorld(world);
      results.push(world);
    }

    reply(results)
      .header('Content-Type', 'application/json')
      .header('Server', 'hapi');
  }

};
