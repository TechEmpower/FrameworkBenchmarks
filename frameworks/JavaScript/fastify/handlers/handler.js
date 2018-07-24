const h = require('../helper');

/**
 * @param databaseLayer
 * @returns {{SingleQuery: function(*), MultipleQueries: function(*), Fortunes: function(*), Updates: function(*)}}
 */
module.exports = (databaseLayer) => ({
  SingleQuery: async (req, reply) => {
    const world = await databaseLayer.getWorldLean(h.randomTfbNumber());

    reply(world);
  },

  MultipleQueries: async (req, reply) => {
    const queries = h.getQueries(ctx.request.query.queries);
    const promisesArray = [];
    for (let i = 0; i < queries; i++) {
      promisesArray.push(databaseLayer.getWorldLean(h.randomTfbNumber()));
    }

    reply(await Promise.all(promisesArray));
  },

  Fortunes: async (req, reply) => {
    const fortunes = await databaseLayer.allFortunes();
    fortunes.push(h.additionalFortune);
    fortunes.sort((a, b) => a.message.localeCompare(b.message));

    reply.view('fortunes.hbs', { fortunes })
  },

  Updates: (req, reply) => {
    const queries = h.getQueries(ctx.request.query.queries);
    const worldPromises = [];

    for (let i = 0; i < queries; i++) {
      worldPromises.push(databaseLayer.getWorld(h.randomTfbNumber()));
    }

    return Promise
      .all(worldPromises)
      .map(world => {
        world.randomNumber = h.randomTfbNumber();
        return world
      })
      .then(worlds => databaseLayer.saveWorlds(worlds))
      .then(worlds => {
        reply(worlds);
      });
  }
});