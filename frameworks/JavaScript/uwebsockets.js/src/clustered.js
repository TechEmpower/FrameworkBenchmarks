import cluster from 'node:cluster';
import process from 'node:process';
import os from 'node:os';

if (cluster.isPrimary) {
    // Master Node
    console.log(`Primary ${process.pid} is running`);

    // Fork workers.
    const numCPUs = os.cpus().length;
    for (let i = 0; i < numCPUs; i++) {
        cluster.fork();
    }

    cluster.on('exit', (worker, code, signal) => {
        console.log(`worker ${worker.process.pid} died`);
        process.exit(1);
    });
} else {
    // Cluster Node
    await import('./server.js');
    console.log(`Worker ${process.pid} started`);
}
