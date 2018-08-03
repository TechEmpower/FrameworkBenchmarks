const {Worlds, Fortunes} = require('../models/mongoose');
const Bluebird = require('bluebird');

module.exports = {
  getWorld: _id => Worlds.findById(_id),
  getWorldLean: _id => Worlds.findById(_id).lean(),
  allFortunes: () => Fortunes.find({}),
  saveWorlds: worlds => Bluebird.all(worlds.map(world => world.save())),
};