const h = require('../helper');
const MongoClient = require('mongodb').MongoClient;
let collections = null, connecting = false, connectionCallbacks = [];

const mongoUrl = 'mongodb://tfb-database:27017';
const dbName = 'hello_world';

/**
 * Note! The benchmarks say we should use "id" as a property name.
 * However, Mongo provides a default index on "_id", so to be equivalent to the other tests, we use
 * the same, default index provided by the database.
 *
 */

const getCollections = async () => {
  // mongoose creates a queue of requests during connection, so we don't have to wait.
  // however, with the raw driver we need to connect first, or sometimes the test will fail randomly
  if (collections) {
    return collections;
  }
  if (connecting) {
    const promise = new Promise((resolve) => {
      connectionCallbacks.push(resolve);
    });
    return await promise;
  }
  connecting = true;
  const client = await MongoClient.connect(mongoUrl);
  collections = {
    World: null,
    Fortune: null
  };
  collections.World = client.db(dbName).collection('world');
  collections.Fortune = client.db(dbName).collection('fortune');
  return collections;
}

const toClientWorld = (world) => {
  if (world) {
    world.id = world._id;
    delete world._id;
  }
  return world;
};


const mongodbRandomWorld = async () => {
  const collections = await getCollections();
  return toClientWorld(collections.World.findOne({
    _id: h.randomTfbNumber()
  }));
};

const mongodbGetAllFortunes = async () => {
  const collections = await getCollections();
  return await collections.Fortune.find().toArray().map(toClientWorld());
};

async function getUpdateRandomWorld() {
  const world = await collections.World.findOne({_id: h.randomTfbNumber()});
  world.randomNumber = h.randomTfbNumber();
  await collections.World.updateOne({
    _id: world._id
  }, {
    randomNumber: world.randomNumber
  });

  return toClientWorld(world);
}

module.exports = {

  SingleQuery: async (req, res) => {
    const result = await mongodbRandomWorld();
    h.addTfbHeaders(res, 'json');
    res.end(JSON.stringify(result));
  },

  MultipleQueries: async (queryCount, req, res) => {
    const queryFunctions = [];
    for (let i = 0; i < queryCount; i++) {
      queryFunctions.push(mongodbRandomWorld());
    }
    const results = await Promise.all(queryFunctions);

    h.addTfbHeaders(res, 'json');
    res.end(JSON.stringify(results));
  },

  Fortunes: async (req, res) => {
    const fortunes = await mongodbGetAllFortunes();
    fortunes.push(h.additionalFortune());
    fortunes.sort(function (a, b) {
      return a.message.localeCompare(b.message);
    });
    h.addTfbHeaders(res, 'html');
    res.end(h.fortunesTemplate({fortunes}));
  },

  Updates: async (queryCount, req, res) => {
    const promises = [];

    for (let i = 1; i <= queryCount; i++) {
      promises.push(getUpdateRandomWorld());
    }

    h.addTfbHeaders(res, 'json');
    res.end(JSON.stringify(await Promise.all(promises)));
  }

};
