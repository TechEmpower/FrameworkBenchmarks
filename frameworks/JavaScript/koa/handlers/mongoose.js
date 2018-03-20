const {Worlds, Fortunes} = require('../models/mongoose');
const Bluebird = require('bluebird');

module.exports = {
  getWorld: id => Worlds.findOne({id}),
  allFortunes: () => Fortunes.find({}),
  saveWorlds: worlds => Bluebird.all(worlds.map(world => world.save())),
};