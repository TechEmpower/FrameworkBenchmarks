const h = require('../helper');
const MongoClient = require('mongodb').MongoClient;
let collections = null, connecting = false, connectionCallbacks = [];

const mongoUrl = 'mongodb://tfb-database:27017';
const dbName = 'hello_world';

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


const mongodbRandomWorld = async () => {
  const collections = await getCollections();
  const world = collections.World.findOne({
    _id: h.randomTfbNumber()
  });
  world._id = undefined; // remove _id from query response
  return world;
};

const mongodbGetAllFortunes = async () => {
  const collections = await getCollections();
  return await collections.Fortune.find().toArray();
};

const mongodbDriverUpdateQuery = async () => {
  const world = await collections.World.findOne({_id: h.randomTfbNumber()}).exec();
  world.randomNumber = h.randomTfbNumber();
  await collections.World.updateOne({_id: world._id}, {
    $set: {
      randomNumber: world.randomNumber
    }
  });
  return world;
};


module.exports = {

  SingleQuery: async (req, res) => {
    const result = await mongodbRandomWorld();
    h.addTfbHeaders(res, 'json');
    res.end(JSON.stringify(result));
  },

  MultipleQueries: async (queryCount, req, res) => {
    const queryFunctions = h.fillArray(mongodbRandomWorld, queryCount);
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
    res.end(h.fortunesTemplate({
      fortunes: fortunes
    }));
  },

  Updates: async (queryCount, req, res) => {
    const queryFunctions = h.fillArray(mongodbDriverUpdateQuery, queryCount);
    const results = await Promise.all(queryFunctions);

    h.addTfbHeaders(res, 'json');
    res.end(JSON.stringify(results));
  }

};
