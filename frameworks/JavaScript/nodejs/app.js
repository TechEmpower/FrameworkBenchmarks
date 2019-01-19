const cluster = require('cluster');
const numCPUs = require('os').cpus().length;

if (cluster.isMaster) {
  // Fork workers.
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', (worker, code, signal) => {
  	console.log([
  	  'A process exit was triggered, most likely due to a failed database action',
  	  'NodeJS test server shutting down now'].join('\n'));
    process.exit(1);
  });
} else {
  // Task for forked worker
  require('./create-server');
}
