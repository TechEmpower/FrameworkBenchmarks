const { configure } = require("chubbajs");
const config = require("./config");
require("regenerator-runtime");

const cluster = require('cluster');
const numCPUs = require('os').cpus().length;

if (cluster.isMaster) {
    // Fork workers.
    for (let i = 0; i < numCPUs; i++) {
        cluster.fork();
    }

    cluster.on('exit', (worker, code, signal) =>
        console.log('worker ' + worker.pid + ' died'));
} else {
    let context;
    async function startServer() {
        context = await configure(config);
        context.app.listen(config.port, () => {
            console.log(`Server is running on port ${config.port}.`);
        });
    }
    startServer();
}

