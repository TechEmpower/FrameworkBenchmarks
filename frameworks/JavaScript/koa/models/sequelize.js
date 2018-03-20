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
  randomNumber: {type: 'Sequelize.INTEGER'}
}, {
  timestamps: false,
  freezeTableName: true
});

const Fortunes = sequelize.define('Fortune', {
  id: {
    type: 'Sequelize.INTEGER',
    primaryKey: true
  },
  message: {type: 'Sequelize.STRING'}
}, {
  timestamps: false,
  freezeTableName: true
});

module.exports = {
  Worlds,
  Fortunes,
};
