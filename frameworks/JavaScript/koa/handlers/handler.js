const h = require('../helper');
const Bluebird = require('bluebird');

/**
 * @param databaseLayer
 * @returns {{SingleQuery: function(*), MultipleQueries: function(*), Fortunes: function(*), Updates: function(*)}}
 */
module.exports = (databaseLayer) => ({
  SingleQuery: async (ctx) => {
    ctx.body = await databaseLayer.getWorldLean(h.randomTfbNumber());
  },

  MultipleQueries: async (ctx) => {
    const queries = h.getQueries(ctx.request.query.queries);
    const promisesArray = [];
    for (let i = 0; i < queries; i++) {
      promisesArray.push(databaseLayer.getWorldLean(h.randomTfbNumber()));
    }
    ctx.body = await Bluebird.all(promisesArray)
  },

  Fortunes: async (ctx) => {
    const fortunes = await databaseLayer.allFortunes();
    fortunes.push(h.additionalFortune);
    fortunes.sort((a, b) => a.message.localeCompare(b.message));

    return ctx.render('fortunes', {fortunes});
  },

  Updates: (ctx) => {
    const queries = h.getQueries(ctx.request.query.queries);
    const worldPromises = [];

    for (let i = 0; i < queries; i++) {
      worldPromises.push(databaseLayer.getWorld(h.randomTfbNumber()));
    }

    return Bluebird
      .all(worldPromises)
      .map(world => {
        world.randomNumber = h.randomTfbNumber();
        return world
      })
      .then(worlds => databaseLayer.saveWorlds(worlds))
      .then(worlds => {
        ctx.body = worlds;
      });
  }
});