const {Worlds, Fortunes} = require('../models/sequelize');
const Bluebird = require('bluebird');

module.exports = {
  getWorld: id => Worlds.findOne({where: {id}}),
  allFortunes: () => Fortunes.findAll(),
  saveWorlds: worlds => Bluebird.all(worlds.map(world => world.save())),
};