const cluster = require( 'cluster' )
const cpus = require( 'os' ).availableParallelism ? require( 'os' ).availableParallelism() : require( 'os' ).cpus().length
const db = require( './db' )

if( cluster.isPrimary || cluster.isMaster ) {
    for( let i = 0; i < cpus; i++ ) {
        cluster.fork();
    }
} else {
    require('uWebSockets.js')._cfg('silent')
    db.init()
        .then( async () => {
                const spliffy = (await import( '@srfnstack/spliffy' )).default
                spliffy(
                    {
                        routeDir: __dirname + '/www',
                        port: 1420,
                        logAccess: false,
                        decodePathParameters: false,
                        parseCookie: false,
                        writeDateHeader: false
                    }
                )
            }
        )
}