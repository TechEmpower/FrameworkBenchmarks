const { MongoClient } = require( "mongodb" );

let World, Fortune;

const dbHost = process.env.DB_HOST || 'localhost'

const projection = { projection: { _id: 0 } }

module.exports = {
    async init() {
        let client = new MongoClient(`mongodb://${dbHost}:27017`, { minPoolSize: 2, maxPoolSize: 75 })
        await client.connect()
        let db = await client.db( 'hello_world' );
        Fortune = await db.collection( 'fortune' );
        World = await db.collection( 'world' );
    },
    allFortunes: async () => Fortune.find( undefined, projection ).toArray(),

    worldById: async ( id ) => World.findOne( { _id: id }, projection ),

    allWorlds: async () => World.find( undefined, projection ).toArray(),

    bulkUpdateWorld: async worlds =>
        World.bulkWrite(
            worlds.map( world => ( {
                updateOne: {
                    filter: {
                        _id: world.id
                    },
                    update: {
                        $set: { randomNumber: world.randomNumber }
                    }
                }
            } ) )
        ).then(()=>worlds)
}
