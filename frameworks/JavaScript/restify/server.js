const cluster = require('cluster');
const numCPUs = require('os').cpus().length;
const server = require('restify').createServer();

server.get('/plaintext', (req, res) =>
  res.send('Hello, World!'));

server.get('/json', (req, res) =>
  res.json({ message: 'Hello, World!' }));

if (cluster.isPrimary) {
  console.log(`Primary ${process.pid} is running`);
  
  // Fork workers.
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }
  
  cluster.on('exit', (worker, code, signal) => {
    console.log(`worker ${worker.process.pid} died`);
  });
} else {
  server.listen(8080, () =>
    console.log(`${server.name} listening at ${server.url}`));
}
