var cluster = require('cluster');
var numCPUs = require('os').cpus().length;

if (cluster.isMaster) {
  // Fork workers.
  for (var i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', function (worker, code, signal) {
  	console.log([
  	  'A process exit was triggered, most likely due to a failed database action',
  	  'NodeJS test server shutting down now'].join('\n'));
    process.exit(1);
  });
} else {
  // Task for forked worker
  require('./create-server');
}
