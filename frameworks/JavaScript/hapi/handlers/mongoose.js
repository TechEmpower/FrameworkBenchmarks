// Connects to MongoDB using the mongoose driver
// Handles related routes

const helper = require('../helper');
const Mongoose = require('mongoose');
Mongoose.connect('mongodb://tfb-database/hello_world', { useNewUrlParser: true, useUnifiedTopology: true });

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

const randomWorld = () => Worlds.findOne({ id: helper.randomTfbNumber() });

const updateWorld = async (world) =>
  await Worlds.updateOne(
    { id: world.randomNumber },
    { randomNumber: world.randomNumber }
  );

module.exports = {

  SingleQuery: async (request, h) => {
    return h.response(await randomWorld()).header('Server', 'hapi');
  },

  MultipleQueries: async (request, h) => {
    const queries = helper.getQueries(request);
    const results = [];

    for (let i = 0; i < queries; i++) {
      results.push(await randomWorld());
    }


    return h.response(results).header('Server', 'hapi');
  },

  Fortunes: async (request, h) => {
    const fortunes = await Fortunes.find();
    fortunes.push(helper.additionalFortune());
    fortunes.sort((a, b) => a.message.localeCompare(b.message));

    return h.view('fortunes', {
      fortunes: fortunes
    })
      .header('Content-Type', 'text/html')
      .header('Server', 'hapi');
  },

  Updates: async (request, h) => {
    const queries = helper.getQueries(request);
    const results = [];

    for (let i = 0; i < queries; i++) {
      const world = await randomWorld();
      world.randomNumber = helper.randomTfbNumber();
      await updateWorld(world);
      results.push(world);
    }

    return h.response(results)
      .header('Content-Type', 'application/json')
      .header('Server', 'hapi');
  }

};
