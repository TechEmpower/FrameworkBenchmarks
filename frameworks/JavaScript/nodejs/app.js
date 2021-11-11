const cluster = require('cluster');
const numCPUs = require('os').cpus().length;

process.env.NODE_HANDLER = 'mysql-raw';

if (process.env.TFB_TEST_NAME === 'nodejs-mongodb') {
  process.env.NODE_HANDLER = 'mongoose';
} else if (process.env.TFB_TEST_NAME === 'nodejs-mongodb-raw') {
  process.env.NODE_HANDLER = 'mongodb-raw';
} else if (process.env.TFB_TEST_NAME === 'nodejs-mysql') {
  process.env.NODE_HANDLER = 'sequelize';
} else if (process.env.TFB_TEST_NAME === 'nodejs-postgres') {
  process.env.NODE_HANDLER = 'sequelize-postgres';
}

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
