const {Worlds, Fortunes} = require('../models/sequelize');

module.exports = {
  getWorld: id => Worlds.findOne({where: {id}}),
  getWorldLean: id => Worlds.findOne({where: {id}, raw: true}),
  allFortunes: () => Fortunes.findAll(),
  saveWorlds: worlds => Promise.all(worlds.map(world => world.save())),
};