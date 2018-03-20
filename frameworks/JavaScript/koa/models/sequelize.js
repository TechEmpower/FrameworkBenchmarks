const Sequelize = require('sequelize');
const sequelize = new Sequelize('hello_world', 'benchmarkdbuser', 'benchmarkdbpass', {
  host: 'TFB-database',
  dialect: 'mysql',
  logging: false
});

const {DataTypes} = Sequelize;
const Worlds = sequelize.define('world', {
  id: {
    type: DataTypes.INTEGER,
    primaryKey: true
  },
  randomNumber: {type: DataTypes.INTEGER}
}, {
  timestamps: false,
  freezeTableName: true
});

const Fortunes = sequelize.define('Fortune', {
  id: {
    type: DataTypes.INTEGER,
    primaryKey: true
  },
  message: {type: DataTypes.STRING}
}, {
  timestamps: false,
  freezeTableName: true
});

module.exports = {
  Worlds,
  Fortunes,
};
