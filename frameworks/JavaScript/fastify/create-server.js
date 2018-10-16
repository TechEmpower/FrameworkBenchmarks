const fastify = require('fastify')();
const handler = require('./handlers/handler');

fastify.register(require('point-of-view'), {
  engine: {
    ejs: require('handlebars')
  },
  templates: __dirname + '/views'
})

fastify.use((req, reply, next) => {
  reply.setHeader('Server', 'Fastify')

  next()
})

fastify.get('/json', (req, reply) => {
  reply.header('Content-Type', 'application/json')
    .code(200)
    .send({ message: 'Hello, World!' });
})

fastify.get('/plaintext', (req, reply) => {
  reply.header('Content-Type', 'text/plain')
    .code(200)
    .send('Hello, World!');
});

const handlerName = process.env.NODE_HANDLER;

if (handlerName) {
  const dbLayer = require(`./handlers/${handlerName}`);

  const routerHandler = handler(dbLayer);

  fastify.get('/db', routerHandler.SingleQuery);
  fastify.get('/queries', routerHandler.MultipleQueries);
  fastify.get('/fortunes', routerHandler.Fortunes);
  fastify.get('/updates', routerHandler.Updates);
}

fastify.listen(8080, '0.0.0.0', err => {
  if (err) {
    throw err;
  }

  console.log(`Worker started and listening on http://0.0.0.0:8080 ${new Date().toISOString()}`);
})
