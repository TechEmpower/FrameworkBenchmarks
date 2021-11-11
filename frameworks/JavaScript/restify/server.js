const cluster = require('cluster');
const cpus = require('os').cpus();
const server = require('restify').createServer();

server.get('/plaintext', (req, res) =>
  res.send('Hello, World!'));

server.get('/json', (req, res) =>
  res.json({ message: 'Hello, World!' }));


if (cluster.isMaster) {
  cpus.forEach(() => cluster.fork());
} else {
  server.listen(8080, () =>
    console.log('%s listening at %s', server.name, server.url));
}
