const db = require('./db')

try {
    db.findWorldById( db.randomId() ).then( console.log )
} catch (e) {
    console.log(e)
    throw new Error(e)
}

require('@srfnstack/spliffy')(
    {
        routeDir: __dirname + '/www',
        port: 1420,
        logAccess: false
    }
)