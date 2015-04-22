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
    logging: true
  });


var World = sequelize.define('World', {
  id: Sequelize.INTEGER,
  randomNumber: Sequelize.INTEGER
},
{
  // prevents sequelize from assuming the table is called 'Worlds'
  freezeTableName: true,
  timestamps: false,
})

var randomWorldId = function() {
  return Math.floor(Math.random() * 10000) + 1;
}

module.exports = {



  /**
   * Test 2: Single Database Query
   */
  single: function (req, res) {
    World.findOne({
      where: { id: randomWorldId() }
    }).then(function(results) {
      return res.json(results.get());
    })
  },


  /**
   * Test 3: Multiple Database Query
   */
  multiple: function (req, res) {
    return res.json({
      todo: 'multiple() is not implemented yet!'
    });
  }
};

