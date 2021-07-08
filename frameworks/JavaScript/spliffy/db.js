const { Pool, native } = require( 'pg' );

const cpus = require( 'os' ).cpus().length

const pool = new Pool( {
    Client: native.Client,
    host: 'tfb-database',
    user: 'benchmarkdbuser',
    password: 'benchmarkdbpass',
    database: 'hello_world',
    //TODO: tune this better, it's not really cpu bound.
    max: cpus * 25
} )
pool.on( 'error', ( err ) => {
    console.error( 'Unexpected client error', err )
} )

let execute = async ( text, values ) => ( await pool.query( text, values || [] ) ).rows;
module.exports = {
    execute,
    randomId: ( min = 1, max = 10000 ) => Math.floor( Math.random() * max ) + min,
    randomUniqueIds: ( count, min = 1, max = 10000 ) => {
        let ids = []
        const used = {}
        for( let i = 0; i < count; i++ ) {
            let id = module.exports.randomId( min, max )
            for( let j = min - 1; id < max; j++ ) {
                if( !used[id] ) {
                    used[id] = true
                    break
                }
                id++
                if( id > used.length - 1 ) {
                    id = min - 1
                }
            }
            ids.push( id )
        }
        return ids
    },

    getAllFortunes: async () =>
        await execute( 'SELECT * FROM fortune' ),

    findWorldById: async ( id ) =>
        await execute( `SELECT *
                             FROM world
                             WHERE id = ?`, [id] )
            .then( arr => arr[0] ),

    getAllCachedWorlds: async () =>
        await execute( 'SELECT * FROM cachedworld' ),

    findCachedWorldById: async ( id ) =>
        await execute( `SELECT *
                             FROM cachedworld
                             WHERE id = ?`, [id] )
            .then( arr => arr[0] ),

    bulkUpdateWorld: async worlds => execute(
        `UPDATE world as w
         SET randomNumber = wv.randomNumber
         FROM (VALUES ${
                 worlds.map( () => `(?,?)` ).join( ',' )
         }) AS wv (id, randomNumber)
         where w.id = wv.id`,
        worlds.map( world => [world.id, world.randomNumber] ).flat()
    )
}