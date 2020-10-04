// Connects to MySQL using the sequelize driver
// Handles related routes

const h = require('../helper');

const Sequelize = require('sequelize');
const sequelize = new Sequelize('hello_world', 'benchmarkdbuser', 'benchmarkdbpass', {
  host: 'tfb-database',
  dialect: 'mysql',
  logging: false
});

const Worlds = sequelize.define('World', {
  id: {
    type: 'Sequelize.INTEGER',
    primaryKey: true
  },
  randomNumber: { type: 'Sequelize.INTEGER' }
}, {
    timestamps: false,
    freezeTableName: true
  });

const Fortunes = sequelize.define('Fortune', {
  id: {
    type: 'Sequelize.INTEGER',
    primaryKey: true
  },
  message: { type: 'Sequelize.STRING' }
}, {
    timestamps: false,
    freezeTableName: true
  });

const randomWorld = async () =>
  await Worlds.findOne({ where: { id: h.randomTfbNumber() } });

module.exports = {

  SingleQuery: async (_req, reply) => {
    const world = await randomWorld();
    return reply
      .response(world)
      .header('Content-Type', 'application/json')
      .header('Server', 'hapi');
  },

  MultipleQueries: async (req, reply) => {
    const queries = h.getQueries(req);
    const promises = [];

    for (let i = 0; i < queries; i++) {
      promises.push(randomWorld());
    }
    const results = await Promise.all(promises);

    return reply
      .response(results)
      .header('Content-Type', 'application/json')
      .header('Server', 'hapi');
  },

  Fortunes: async (_req, reply) => {
    const fortunes = await Fortunes.findAll();
    fortunes.push(h.additionalFortune());
    fortunes.sort((a, b) => a.message.localeCompare(b.message));

    return reply.view('fortunes', { fortunes })
      .header('Content-Type', 'text/html')
      .header('Server', 'hapi');
  },

  Updates: async (req, reply) => {
    const queries = h.getQueries(req);
    const promises = [];

    for (let i = 0; i < queries; i++) {
      const promise = randomWorld()
        .then(world => {
          world.randomNumber = h.randomTfbNumber();
          return Worlds.update(
            { randomNumber: world.randomNumber },
            { where: { id: world.id } }
          ).then(() => world);
        })
      promises.push(promise);
    }
    const results = await Promise.all(promises);

    return reply
      .response(results)
      .header('Content-Type', 'application/json')
      .header('Server', 'hapi');
  }

};
