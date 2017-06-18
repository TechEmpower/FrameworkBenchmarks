// Connects to MySQL using the sequelize driver
// Handles related routes
const Promise = require('bluebird');
const h = require('../helper');

const Sequelize = require('sequelize');
const sequelize = new Sequelize('hello_world', 'benchmarkdbuser', 'benchmarkdbpass', {
  host: 'TFB-database',
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

const randomWorldPromise = () =>
  Worlds.findOne({ where: { id: h.randomTfbNumber() } })
    .then((results) => results)
    .catch((err) => process.exit(1));


module.exports = {

  SingleQuery: (ctx, next) => {
    return randomWorldPromise().then((world) => {
      ctx.set('Server', 'Koa');
      ctx.type = 'application/json';
      ctx.body = world;
      return next();
    })
  },

  MultipleQueries: (ctx, next) => {
    const queries = h.getQueries(ctx);
    const worldPromises = [];

    for (let i = 0; i < queries; i++) {
      worldPromises.push(randomWorldPromise());
    }

    return Promise.all(worldPromises).then((worlds) => {
      ctx.set('Server', 'Koa');
      ctx.type = 'application/json';
      ctx.body = worlds;
      return next();
    });
  },

  Fortunes: (ctx, next) => {
    return Fortunes.findAll().then((fortunes) => {
      fortunes.push(h.additionalFortune());
      fortunes.sort((a, b) => a.message.localeCompare(b.message));
      
      ctx.set('Server', 'Koa');
      ctx.type = 'text/html';
      return ctx.render('fortunes', { fortunes });
    }).catch((err) => process.exit(1));
  },

  Updates: (ctx, next) => {
    const queries = h.getQueries(ctx);
    const worldPromises = [];

    for (let i = 0; i < queries; i++) {
      worldPromises.push(randomWorldPromise());
    }

    const worldUpdate = (world) => {
      world.randomNumber = h.randomTfbNumber();

      return Worlds.update(
          { randomNumber: world.randomNumber },
          { where: { id: world.id } }
        )
        .then((results) => world)
        .catch((err) => process.exit(1));
    };

    return Promise
      .all(worldPromises)
      .map((world) => worldUpdate(world))
      .then((updated) => {
        ctx.set('Server', 'Koa');
        ctx.type = 'application/json';
        ctx.body = updated;
        return next();
      })
      .catch((e) => process.exit(1));
  }
};
