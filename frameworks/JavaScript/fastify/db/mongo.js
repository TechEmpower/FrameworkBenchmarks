const { MongoClient } = require("mongodb");

const mongoUrl = "mongodb://tfb-database:27017";
const dbName = "hello_world";

let client;

async function getCollection(name) {
  if (!client) {
    client = await new MongoClient(
      mongoUrl,
      {
        useNewUrlParser: true,
        useUnifiedTopology: true
      }
    ).connect();
  }

  const db = client.db(dbName);

  return db.collection(name);
}

async function allFortunes() {
  const collection = await getCollection("fortune");
  const fortunes = await collection.find({}, { projection: { _id: 0 } });

  return fortunes.toArray();
}

async function getWorld(id) {
  const collection = await getCollection("world");

  return collection.findOne({ id }, { projection: { _id: 0 } });
}

async function saveWorlds(worlds) {
  const collection = await getCollection("world");

  const bulk = collection.initializeUnorderedBulkOp();

  worlds.forEach(world => {
    bulk.find({ id: world.id }).updateOne(world);
  });

  return bulk.execute();
}

module.exports = {
  getWorld,
  saveWorlds,
  allFortunes
};
