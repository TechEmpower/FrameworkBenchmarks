const h = require("./helper");

/**
 * @param databaseLayer
 * @returns {{singleQuery: function(*), multipleQueries: function(*), fortunes: function(*), updates: function(*)}}
 */
module.exports = databaseLayer => ({
  singleQuery: async (req, reply) => {
    const world = await databaseLayer.getWorld(h.randomTfbNumber());

    reply.send(world);
  },

  multipleQueries: async (req, reply) => {
    const queries = h.getQueries(req.query.queries);
    const promisesArray = [];

    for (let i = 0; i < queries; i++) {
      promisesArray.push(databaseLayer.getWorld(h.randomTfbNumber()));
    }

    const worlds = await Promise.all(promisesArray);

    reply.send(worlds);
  },

  fortunes: async (req, reply) => {
    const fortunes = await databaseLayer.allFortunes();

    fortunes.push(h.additionalFortune);
    fortunes.sort(compare);

    return reply.view("/views/fortunes.hbs", { fortunes });
  },

  updates: async (req, reply) => {
    const queries = h.getQueries(req.query.queries);
    const worldPromises = [];

    for (let i = 0; i < queries; i++) {
      worldPromises.push(databaseLayer.getWorld(h.randomTfbNumber()));
    }

    const worlds = await Promise.all(worldPromises);

    const worldsToUpdate = worlds.map(world => {
      world.randomNumber = h.randomTfbNumber();
      return world;
    });

    await databaseLayer.saveWorlds(worldsToUpdate);

    reply.send(worldsToUpdate);
  }
});

// faster than localeCompare
function compare(a, b) {
  if(a.message < b.message){
    return -1;
  } else if (a.message > b.message) {
    return 1;
  }
  return 0;
}
