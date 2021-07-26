const { MongoClient } = require( "mongodb" );

let World, Fortune;

const dbHost = process.env.DB_HOST || 'localhost'

const projection = { projection: { _id: 0 } }



module.exports = {
    async init() {
        let client = new MongoClient(`mongodb://${dbHost}:27017`, { minPoolSize: 2, maxPoolSize: 35 })
        await client.connect()
        let db = await client.db( 'hello_world' );
        Fortune = await db.collection( 'fortune' );
        World = await db.collection( 'world' );
    },
    allFortunes: async () => Fortune.find( {}, projection ).toArray(),

    worldById: async ( id ) => World.findOne( { id }, projection ),

    allWorlds: async () => Fortune.find( {}, projection ).toArray(),

    bulkUpdateWorld: async worlds =>
        World.bulkWrite(
            worlds.map( world => ( {
                updateOne: {
                    filter: {
                        _id: world.id
                    },
                    update: {
                        $set: { randomnumber: world.randomnumber }
                    }
                }
            } ) )
        ).then((res)=>worlds)
}
