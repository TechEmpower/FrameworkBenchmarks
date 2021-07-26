const { Client, Pool } = require( 'pg' ).native;
const cpus = require( 'os' ).cpus().length

let clientOpts = {
    host: process.env.DB_HOST || 'localhost',
    user: 'benchmarkdbuser',
    password: 'benchmarkdbpass',
    database: 'hello_world'
};

let pool

let query = async ( text, values ) => ( await pool.query( text, values || undefined ) ).rows

module.exports = {
    async init() {
        const client = new Client( clientOpts )
        await client.connect()
        const res = await client.query( 'SHOW max_connections' )
        let maxConnections = Math.floor( res.rows[0].max_connections * 0.9 / cpus )
        //1 worker per cpu, each worker pool gets a fraction of the max connections
        //only use 90% to avoid too many clients errors
        pool = new Pool( Object.assign( { ...clientOpts }, { max: maxConnections } ) )
        pool.on( 'error', ( err ) => {
            console.error( 'Unexpected client error', err )
        } )
        await client.end()
    },
    allFortunes: async () =>
        query( 'SELECT * FROM fortune' ),

    worldById: async ( id ) =>
        query( `SELECT *
                FROM world
                WHERE id = $1`, [id] )
            .then( arr => arr[0] ),

    allWorlds: async () =>
        query( 'SELECT * FROM world' ),

    bulkUpdateWorld: async worlds => {
        let args = []
        for( let world of worlds ) {
            args.push( world.id, world.randomnumber )
        }
        return query(
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
            args )
            .then( () => worlds )
    }
}
