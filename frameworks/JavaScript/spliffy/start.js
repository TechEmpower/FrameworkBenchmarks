const cluster = require('cluster')
const cpus = require('os').cpus().length

if (cluster.isMaster) {
    for (let i = 0; i < cpus; i++) {
        cluster.fork();
    }
} else {
    require('@srfnstack/spliffy')(
        {
            routeDir: __dirname + '/www',
            port: 1420,
            logAccess: false
        }
    )
}
