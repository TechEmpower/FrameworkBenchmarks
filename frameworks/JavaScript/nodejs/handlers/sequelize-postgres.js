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
  id:      { type: 'Sequelize.INTEGER' },
  message: { type: 'Sequelize.STRING' }
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

  SingleQuery: function (req, res) {
    randomWorldPromise().then(function (world) {
      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(world));
    });
  },

  MultipleQueries: function (queries, req, res) {
    var worldPromises = [];

    for (var i = 0; i < queries; i++) {
      worldPromises.push(randomWorldPromise());
    } 

    Promise.all(worldPromises).then(function (worlds) {
      h.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(worlds));
    });
  },

  Fortunes: function (req, res) {
    Fortunes.findAll().then(function (fortunes) {
      fortunes.push(h.ADDITIONAL_FORTUNE);
      fortunes.sort(function (a, b) {
        return a.message.localeCompare(b.message);
      });

      h.addTfbHeaders(res, 'html');
      res.end(h.fortunesTemplate({
        fortunes: fortunes
      }));
    }).catch(function (err) {
      console.log(err.stack);
      process.exit(1);
    });
  },

  Updates: function (queries, req, res) {
    var worldPromises = [];

    for (var i = 0; i < queries; i++) {
      worldPromises.push(randomWorldPromise());
    }

    var worldUpdate = function(world) {
      world.randomNumber = h.randomTfbNumber();

      return Worlds.update({
        randomNumber: world.randomNumber
      },
      {
        where: { id: world.id }
      }).then(function (results) {
        return world;
      }).catch(function (err) {
        process.exit(1);
      });
    }

    Promise.all(worldPromises).then(function (worlds) {
      var updates = worlds.map(function (e) {
        return worldUpdate(e);
      });

      Promise.all(updates).then(function (updated) {
        h.addTfbHeaders(res, 'json');
        res.end(JSON.stringify(updated));
      });
    });
  }

}