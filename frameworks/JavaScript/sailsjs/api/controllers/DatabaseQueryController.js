/**
 * DatabaseQueryController
 *
 * @description :: Server-side logic for managing Database Queries
 */

var Sequelize = require('sequelize')
var sequelize = new Sequelize(
  'hello_world', 'benchmarkdbuser', 'benchmarkdbpass',
  {
    host: '127.0.0.1',
    dialect: 'mysql',
    pool: {
      max: 5000,
      min: 0,
      idle: 5000
    },
    // hide the SQL queries being run
    logging: false
  });


var World = sequelize.define('World', {
  id: Sequelize.INTEGER,
  randomNumber: Sequelize.INTEGER
},
{
  // prevents sequelize from assuming the table is called 'Worlds'
  freezeTableName: true,
  timestamps: false
})


var Fortune = sequelize.define('Fortune', {
  id: Sequelize.INTEGER,
  message: Sequelize.STRING
},
{
  // prevents sequelize from assuming the table is called 'Worlds'
  freezeTableName: true,
  timestamps: false
})


var randomTFBnumber = function() {
  return Math.floor(Math.random() * 10000) + 1;
}


var worldQuery = function(callback) {
  World.findOne({
    where: { id: randomTFBnumber() }
  }).complete(callback);
}


var worldUpdate = function(world, callback) {
  World.update({
      randomNumber: world.randomNumber
    },
    {
      where: {
        id: world.id
      }
    }
  ).complete(callback);
}


module.exports = {

  /**
   * Test 2: Single Database Query
   */
  single: function (req, res) {
    World.findOne({
      where: { id: randomTFBnumber() }
    }).then(function(results) {
      return res.json(results.get());
    })
  },


  /**
   * Test 3: Multiple Database Query
   */
  multiple: function (req, res) {
    var queries = req.param('queries');
    var toRun = [];

    queries = Math.min(Math.max(queries, 1), 500) || 1;

    for (var i = 0; i < queries; i++) {
      toRun.push(worldQuery);
    }

    async.parallel(toRun, function(err, results) {
      if (!err) {
        res.json(results);
      } else {
        res.badRequest('Unexpected failure to fetch multiple records.');
      }
    });
  },


  /**
   * Test 4: Fortunes
   */
  fortunes: function(req, res) {
    Fortune.findAll().then(function (fortunes) {
      fortunes.push({
        id: 0,
        message: "Additional fortune added at request time."
      });
      fortunes.sort(function (a, b) {
        return a.message.localeCompare(b.message);
      });
      return res.render('fortunes', { 'fortunes': fortunes });
    })
  },


  /**
   * Test 5: Database Updates
   */
  updates: function(req, res) {
    var queries = req.param('queries');
    var worlds = [];

    queries = Math.min(Math.max(queries, 1), 500) || 1;

    for (var i = 0; i < queries; i++) {
      worlds.push({
        id: randomTFBnumber(),
        randomNumber: randomTFBnumber()
      });
    }

    async.map(worlds, worldUpdate, function(err, results) {
      if (!err) {
        res.json(worlds);
      } else {
        res.badRequest('Unexpected failure to update records.');
      }
    });
  }
  
};

