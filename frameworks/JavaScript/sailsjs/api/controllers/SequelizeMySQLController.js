/**
 * SequelizeMySQLController
 *
 * @description :: Server-side logic for managing Database Queries
 */

var h = require('../services/helper')
var Promise = require('bluebird')

var Sequelize = require('sequelize')
var sequelize = new Sequelize(
  'hello_world', 'benchmarkdbuser', 'benchmarkdbpass',
  {
    host: 'tfb-database',
    dialect: 'mysql',
    pool: {
      max: 5000,
      min: 0,
      idle: 5000
    },
    // hide the SQL queries being run
    logging: false
  })


var Worlds = sequelize.define('world', {
  id: {
    type: Sequelize.INTEGER,
    primaryKey: true
  },
  randomNumber: Sequelize.INTEGER
},
  {
    // prevents sequelize from assuming the table is called 'Worlds'
    freezeTableName: true,
    timestamps: false
  })


var Fortunes = sequelize.define('Fortune', {
  id: {
    type: Sequelize.INTEGER,
    primaryKey: true
  },
  message: Sequelize.STRING
},
  {
    // prevents sequelize from assuming the table is called 'Fortunes'
    freezeTableName: true,
    timestamps: false
  })


var randomWorldPromise = function () {
  var promise = Worlds
    .findOne({
      where: { id: h.randomTfbNumber() }
    })
    .then(function (world) {
      return world
    })
    .catch(function (err) {
      process.exit(1)
    })
  return promise
}

var updateWorld = function (world) {
  world.randomNumber = h.randomTfbNumber()
  var promise = Worlds
    .update(
      { randomNumber: world.randomNumber },
      { where: { id: world.id } }
    )
    .then(function (results) {
      return world
    })
    .catch(function (err) {
      process.exit(1)
    })
  return promise
}


module.exports = {

  /**
   * Test 2: Single Database Query
   */
  Single: function (req, res) {
    randomWorldPromise()
      .then(function (world) {
        res.json(world)
      })
  },


  /**
   * Test 3: Multiple Database Query
   */
  Multiple: function (req, res) {
    var queries = h.getQueries(req)
    var toRun = []

    for (var i = 0; i < queries; i++) {
      toRun.push(randomWorldPromise());
    }

    Promise
      .all(toRun)
      .then(function (worlds) {
        res.json(worlds)
      })
  },


  /**
   * Test 4: Fortunes
   */
  Fortunes: function (req, res) {
    Fortunes
      .findAll()
      .then(function (fortunes) {
        fortunes.push(h.additionalFortune())
        fortunes.sort(function (a, b) {
          return a.message.localeCompare(b.message)
        })
        res.render('fortunes', { 'fortunes': fortunes })
      })
      .catch(function (err) {
        process.exit(1)
      })
  },


  /**
   * Test 5: Database Updates
   */
  Updates: function (req, res) {
    var queries = h.getQueries(req);
    var worldPromises = [];

    for (var i = 0; i < queries; i++) {
      worldPromises.push(randomWorldPromise())
    }

    Promise
      .all(worldPromises)
      .map(function (world) {
        return updateWorld(world)
      })
      .then(function (updated) {
        res.json(updated)
      })
      .catch(function (err) {
        process.exit(1)
      })
  }

};

