const { Worlds, Fortunes } = require('../models/mongoose');

module.exports = {
  getWorld: _id => Worlds.findById(_id),
  getWorldLean: _id => Worlds.findById(_id).lean(),
  allFortunes: () => Fortunes.find({}),
  saveWorlds: worlds => Promise.all(worlds.map(world => world.save())),
};