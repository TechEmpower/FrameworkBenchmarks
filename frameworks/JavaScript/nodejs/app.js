const cluster = require('node:cluster');
const { availableParallelism } = require('node:os');
const numCPUs = availableParallelism();

process.env.NODE_HANDLER = 'postgres';

if (process.env.TFB_TEST_NAME === 'nodejs-mongodb') {
  process.env.NODE_HANDLER = 'mongoose';
} else if (process.env.TFB_TEST_NAME === 'nodejs-mongodb-raw') {
  process.env.NODE_HANDLER = 'mongodb-raw';
} else if (process.env.TFB_TEST_NAME === 'nodejs-mysql') {
  process.env.NODE_HANDLER = 'sequelize';
} else if (process.env.TFB_TEST_NAME === 'nodejs-mysql-raw') {
  process.env.NODE_HANDLER = 'mysql-raw';
} else if (process.env.TFB_TEST_NAME === 'nodejs-postgres') {
  process.env.NODE_HANDLER = 'sequelize-postgres';
} else if (process.env.TFB_TEST_NAME === 'nodejs-postgresjs-raw') {
  process.env.NODE_HANDLER = 'postgres';
}

if (numCPUs > 1 && cluster.isPrimary) {
  console.log(`Primary ${process.pid} is running`);

  // Fork workers.
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }
  
  cluster.on('exit', (worker, code, signal) => {
    console.log(`worker ${worker.process.pid} died`);
    process.exit(1);
  });
} else {
  // Task for forked worker
  require('./create-server');
}
