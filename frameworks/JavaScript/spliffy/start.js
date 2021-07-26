const cluster = require( 'cluster' )
const cpus = require( 'os' ).cpus().length
const db = require( './db' )

const init = ()=> db.init()
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

if(process.env.USE_CLUSTER){
    if( cluster.isMaster ) {
        for( let i = 0; i < cpus; i++ ) {
            cluster.fork();
        }
    } else {
        init()
    }
} else {
    init()
}
