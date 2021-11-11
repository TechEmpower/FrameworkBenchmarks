const {Worlds, Fortunes} = require('../models/sequelize-postgres');
const Bluebird = require('bluebird');

module.exports = {
  getWorld: id => Worlds.findOne({where: {id}}),
  getWorldLean: id => Worlds.findOne({where: {id}, raw: true}),
  allFortunes: () => Fortunes.findAll(),
  saveWorlds: worlds => Bluebird.all(worlds.map(world => world.save())),
};