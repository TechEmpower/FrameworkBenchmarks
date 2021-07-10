const { Pool, native } = require( 'pg' );
const cpus = require( 'os' ).cpus().length

const pool = new Pool( {
    Client: native.Client,
    host: 'tfb-database',
    // host: 'host.docker.internal',
    // host: 'localhost',
    user: 'benchmarkdbuser',
    password: 'benchmarkdbpass',
    database: 'hello_world',
    //postgres max_connections = 2000
    //1 worker per cpu
    max: Math.floor( 2000 / cpus )
} )
pool.on( 'error', ( err ) => {
    console.error( 'Unexpected client error', err )
} )

let execute = async ( text, values ) => {
    try {
        return ( await pool.query( text, values || undefined ) ).rows;
    } catch( e ) {
        throw new Error( e )
    }
}
module.exports = {
    execute,
    randomId: () => Math.floor( Math.random() * 10000 ) + 1,
    randomUniqueIds: ( count ) => {
        const ids = {}
        for( let i = 0; i < count; i++ ) {
            let id = module.exports.randomId()
            if( ids[id] ) {
                for( let j = 0; j < 10000 - 1; j++ ) {
                    if( !ids[id] ) break
                    id++
                    if( id > 10000 ) {
                        id = 1
                    }
                }
            }
            ids[id] = true
        }
        return Object.keys( ids )
    },

    allFortunes: async () =>
        execute( 'SELECT * FROM fortune' ),

    worldById: async ( id ) =>
        execute( `SELECT *
                  FROM world
                  WHERE id = $1`, [id] )
            .then( arr => arr[0] ),

    allWorlds: async () =>
        execute( 'SELECT * FROM world' ),

    bulkUpdateWorld: async worlds =>
        execute(
            `UPDATE world as w
             SET randomnumber = wc.randomnumber
             FROM (
                      SELECT win.id, win.randomnumber
                      FROM world wb,
                           (VALUES ${
                                   //0 -> 1,2 ; 1 -> 3,4; 2 -> 5,6; 3 -> 7,8 ... = (i+1) * 2 - 1, (i+1) * 2
                                   worlds.map( ( _, i ) => `(\$${( i + 1 ) * 2 - 1}::int,$${( i + 1 ) * 2}::int)` ).join( ',' )
                           }) AS win (id, randomnumber)
                      WHERE wb.id = win.id
                          FOR UPDATE
                  ) as wc
             where w.id = wc.id`,
            worlds.map( world => [world.id, world.randomnumber] ).flat() )
            .then( () => worlds )
}
