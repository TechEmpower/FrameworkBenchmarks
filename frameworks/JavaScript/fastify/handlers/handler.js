const h = require('../helper');

/**
 * @param databaseLayer
 * @returns {{SingleQuery: function(*), MultipleQueries: function(*), Fortunes: function(*), Updates: function(*)}}
 */
module.exports = (databaseLayer) => ({
  SingleQuery: async (req, reply) => {
    const world = await databaseLayer.getWorldLean(h.randomTfbNumber());

    reply.send(world);
  },

  MultipleQueries: async (req, reply) => {
    const queries = h.getQueries(req.query.queries);
    const promisesArray = [];

    for (let i = 0; i < queries; i++) {
      promisesArray.push(databaseLayer.getWorldLean(h.randomTfbNumber()));
    }

    reply.send(await Promise.all(promisesArray));
  },

  Fortunes: async (req, reply) => {
    const fortunes = await databaseLayer.allFortunes();
    fortunes.push(h.additionalFortune);
    fortunes.sort((a, b) => a.message.localeCompare(b.message));

    reply.view('fortunes.hbs', { fortunes })
  },

  Updates: async (req, reply) => {
    const queries = h.getQueries(req.query.queries);
    const worldPromises = [];

    for (let i = 0; i < queries; i++) {
      worldPromises.push(databaseLayer.getWorld(h.randomTfbNumber()));
    }

    const worlds = await Promise.all(worldPromises);

    const worldsToUpdate = worlds.map(world => {
      world.randomNumber = h.randomTfbNumber();
      return world
    });

    const updatedWorlds = await databaseLayer.saveWorlds(worldsToUpdate);

    reply.send(updatedWorlds);
  }
});