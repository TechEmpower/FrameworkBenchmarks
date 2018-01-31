// Connects to Postgres using the sequelize driver
// Handles related routes

const Promise = require('bluebird');
const h = require('../helper');
const Sequelize = require('sequelize');

const sequelize = new Sequelize('hello_world', 'benchmarkdbuser', 'benchmarkdbpass', {
  host: 'TFB-database',
  dialect: 'postgres',
  logging: false
});

const Worlds = sequelize.define('world', {
  id: {
    type: 'Sequelize.INTEGER',
    primaryKey: true
  },
  randomnumber: { type: 'Sequelize.INTEGER' }
}, {
  timestamps: false,
  freezeTableName: true
});

const Fortunes = sequelize.define('Fortune', {
  id: {
    type: 'Sequelize.INTEGER',
    primaryKey: true
  },
  message:      { type: 'Sequelize.STRING' }
}, {
  timestamps: false,
  freezeTableName: true
});

const randomWorldPromise = () =>
  Worlds.findOne({ where: { id: h.randomTfbNumber() } })
    .then((results) => results)
    .catch((err) => process.exit(1));

module.exports = {

  SingleQuery: (req, reply) =>
    randomWorldPromise().then((world) => reply(world).header('Server', 'hapi')),

  MultipleQueries: (req, reply) => {
    const queries = h.getQueries(req),
      worldPromises = [];

    for (let i = 0; i < queries; i++) {
      worldPromises.push(randomWorldPromise());
    }

    Promise.all(worldPromises)
      .then((worlds) => reply(worlds).header('Server', 'hapi'));
  },

  Fortunes: (req, reply) => {
    Fortunes.findAll().then((fortunes) => {
      fortunes.push(h.additionalFortune());
      fortunes.sort((a, b) => a.message.localeCompare(b.message));

      reply.view('fortunes', { fortunes: fortunes })
        .header('Content-Type', 'text/html')
        .header('Server', 'hapi');
    }).catch((err) => process.exit(1));
  },

  Updates: (req, reply) => {
    const queries = h.getQueries(req),
      worldPromises = [];

    for (let i = 0; i < queries; i++) {
      worldPromises.push(randomWorldPromise());
    }

    const worldUpdate = (world) => {
      world.randomNumber = h.randomTfbNumber();

      return Worlds.update(
        {randomNumber: world.randomNumber},
        {where: {id: world.id}}
      )
        .then((results) => world)
        .catch((err) => process.exit(1));
    };

    Promise
      .all(worldPromises)
      .map((world) => worldUpdate(world))
      .then((updated) => reply(updated).header('Server', 'hapi'))
      .catch((err) => process.exit(1));
  }

};
