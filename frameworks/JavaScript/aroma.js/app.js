
const cluster = require('cluster'),
  numCPUs = require('os').cpus().length,
  Aroma = require('aroma.js');


if (cluster.isPrimary) {
  console.log(`Primary ${process.pid} is running`);

  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', (worker, code, signal) => {
    console.log(`worker ${worker.process.pid} died`);
  });
} else {
  const app = module.exports = new Aroma();

  app.parseUrlEncoded();

   app.use((req, res, next) => {
    res.setHeader("Server", "Aroma.js");
    return next();
  });

  app.get('/json', (req, res) => res.send({ message: 'Hello, World!' }));

  app.get('/plaintext', (req, res) => {
    res.setHeader('Content-Type', 'text/plain');
    res.send('Hello, World!');
  });
  

  app.listen(8080);
}
