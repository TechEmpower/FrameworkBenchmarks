// Connects to Postgres using the sequelize driver
// Handles related routes

var h = require('../helper');
var Promise = require('bluebird');
var Sequelize = require('sequelize');

var sequelize = new Sequelize('hello_world', 'benchmarkdbuser', 'benchmarkdbpass', {
  host: '127.0.0.1',
  dialect: 'postgres',
  logging: false
});

var Worlds = sequelize.define('World', {
  id:           { type: 'Sequelize.INTEGER' },
  randomnumber: { type: 'Sequelize.INTEGER' }
}, {
  timestamps: false,
  freezeTableName: true
});

var Fortunes = sequelize.define('Fortune', {
  id:           { type: 'Sequelize.INTEGER' },
  message:      { type: 'Sequelize.STRING' }
}, {
  timestamps: false,
  freezeTableName: true
});

var randomWorldPromise = function() {
  return Worlds.findOne({
    where: { id: h.randomTfbNumber() }
  }).then(function (results) {
    return results;
  }).catch(function (err) {
    process.exit(1);
  });
}

module.exports = {

  SingleQuery: function (req, reply) {
    randomWorldPromise().then(function (world) {
      reply(world)
        .header('Server', 'hapi');
    })
  },

  MultipleQueries: function (req, reply) {
    var queries = h.getQueries(req);
    var worldPromises = [];

    for (var i = 0; i < queries; i++) {
      worldPromises.push(randomWorldPromise());
    }

    Promise.all(worldPromises).then(function (worlds) {
      reply(worlds)
        .header('Server', 'hapi');
    });
  },

  Fortunes: function (req, reply) {
    Fortunes.findAll().then(function (fortunes) {
      fortunes.push(h.ADDITIONAL_FORTUNE);
      fortunes.sort(function (a, b) {
        return a.message.localeCompare(b.message);
      });

      reply.view('fortunes', {
        fortunes: fortunes
      })
        .header('Content-Type', 'text/html')
        .header('Server', 'hapi');
    }).catch(function (err) {
      process.exit(1);
    }); 
  },

  Updates: function (req, reply) {
    var queries = h.getQueries(req);
    var worldPromises = [];

    for (var i = 0; i < queries; i++) {
      worldPromises.push(randomWorldPromise());
    }

    var worldUpdate = function (world) {
      world.randomNumber = h.randomTfbNumber();

      return Worlds.update(
          { randomNumber: world.randomNumber },
          { where: { id: world.id } }
        )
        .then(function (results) {
          return world;
        })
        .catch(function (err) {
          process.exit(1);
        });
    }

    Promise
      .all(worldPromises)
      .map(function (world) {
        return worldUpdate(world);
      })
      .then(function (updated) {
        reply(updated)
          .header('Server', 'hapi')
      })
      .catch(function (err) {
        process.exit(1);
      });
  }

};