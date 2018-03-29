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

  SingleQuery: (req, reply) => {
    reply(randomWorld())
      .header('Content-Type', 'application/json')
      .header('Server', 'hapi');
  },

  MultipleQueries: async (req, reply) => {
    const queries = h.getQueries(req);
    const results = [];

    for (let i = 0; i < queries; i++) {
      results.push(await randomWorld());
    }

    reply(results)
      .header('Content-Type', 'application/json')
      .header('Server', 'hapi');
  },

  Fortunes: (req, reply) => {
    Fortunes.findAll().then((fortunes) => {
      fortunes.push(h.additionalFortune());
      fortunes.sort((a, b) => a.message.localeCompare(b.message));

      reply.view('fortunes', { fortunes })
        .header('Content-Type', 'text/html')
        .header('Server', 'hapi');
    }).catch((err) => process.exit(1));
  },

  Updates: async (req, reply) => {
    const queries = h.getQueries(req);
    const results = [];

    for (let i = 0; i < queries; i++) {
      const world = await randomWorld();
      world.randomNumber = h.randomTfbNumber();
      await Worlds.update(
        { randomNumber: world.randomNumber },
        { where: { id: world.id } }
      );
      results.push(world);
    }

    reply(results)
      .header('Content-Type', 'application/json')
      .header('Server', 'hapi');
  }

};
