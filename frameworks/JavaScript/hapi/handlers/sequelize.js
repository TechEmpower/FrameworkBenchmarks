// Connects to MySQL using the sequelize driver
// Handles related routes

const helper = require('../helper');

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

const randomWorld = () => Worlds.findOne({ where: { id: helper.randomTfbNumber() } });

module.exports = {

  SingleQuery: async (request, h) => {
    return h.response(await randomWorld())
      .header('Content-Type', 'application/json')
      .header('Server', 'hapi');
  },

  MultipleQueries: async (request, h) => {
    const queries = helper.getQueries(request);
    const results = [];

    for (let i = 0; i < queries; i++) {
      results.push(await randomWorld());
    }

    return h.response(results)
      .header('Content-Type', 'application/json')
      .header('Server', 'hapi');
  },

  Fortunes: async (request, h) => {
    try {
      const fortunes = await Fortunes.findAll()
      fortunes.push(helper.additionalFortune());
      fortunes.sort((a, b) => a.message.localeCompare(b.message));

      return h.view('fortunes', { fortunes })
        .header('Content-Type', 'text/html')
        .header('Server', 'hapi');
    } catch (err) {
      process.exit(1)
    }
  },

  Updates: async (request, h) => {
    const queries = helper.getQueries(request);
    const results = [];

    for (let i = 0; i < queries; i++) {
      const world = await randomWorld();
      world.randomNumber = helper.randomTfbNumber();
      await Worlds.update(
        { randomNumber: world.randomNumber },
        { where: { id: world.id } }
      );
      results.push(world);
    }

    return h.response(results)
      .header('Content-Type', 'application/json')
      .header('Server', 'hapi');
  }

};
