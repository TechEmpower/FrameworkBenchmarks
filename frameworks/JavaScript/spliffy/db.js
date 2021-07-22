const { Pool, native } = require( 'pg' );
const cpus = require( 'os' ).cpus().length
let maxConnections
let clientOpts = {
    Client: native.Client,
    host: 'tfb-database',
    // host: 'host.docker.internal',
    // host: 'localhost',
    user: 'benchmarkdbuser',
    password: 'benchmarkdbpass',
    database: 'hello_world'
};

let pool

let execute = async ( text, values ) => {
    try {
        return ( await pool.query( text, values || undefined ) ).rows;
    } catch( e ) {
        throw new Error( e )
    }
}
module.exports = {
    async init() {
        const client = new native.Client( clientOpts )
        await client.connect()
        const res = await client.query( 'SHOW max_connections' )
        maxConnections = res.rows[0].max_connections
        //1 worker per cpu, each worker pool gets a fraction of the max connections
        //only use 90% to avoid too many clients errors
        pool = new Pool( Object.assign( { ...clientOpts }, { max: Math.floor( maxConnections * 0.9 / cpus ) } ) )
        pool.on( 'error', ( err ) => {
            console.error( 'Unexpected client error', err )
        } )
        await client.end()
    },
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
