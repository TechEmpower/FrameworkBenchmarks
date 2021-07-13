const cluster = require( 'cluster' )
const cpus = require( 'os' ).cpus().length
const db = require( './db' )

if( cluster.isMaster ) {
    for( let i = 0; i < cpus; i++ ) {
        cluster.fork();
    }
} else {
    db.init()
        .then( () => {
                require( '@srfnstack/spliffy' )(
                    {
                        routeDir: __dirname + '/www',
                        port: 1420,
                        logAccess: false
                    }
                )
            }
        )
}
