const { Worlds, Fortunes } = require('../models/mongoose');

module.exports = {
  getWorld: _id => Worlds.findById(_id),
  getWorldLean: _id => Worlds.findById(_id).select('-_id').lean(),
  allFortunes: () => Fortunes.find({}).select('-_id'),
  saveWorlds: async (worlds) => {
    const updatedWorlds = await Promise.all(worlds.map(world => world.save()))

    return updatedWorlds.map((w) => ({ id: w._id, randomNumber: w.randomNumber }))
  }
};