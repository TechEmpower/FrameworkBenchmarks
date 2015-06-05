var cluster = require('cluster');
var numCPUs = require('os').cpus().length;

if (cluster.isMaster) {
  // Fork workers.
  for (var i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  console.log('Master starting ' + new Date().toISOString(" "));
  cluster.on('exit', function (worker, code, signal) {
    process.exit(1);
  });
} else {
  // worker task
  require('./create-server');
}
