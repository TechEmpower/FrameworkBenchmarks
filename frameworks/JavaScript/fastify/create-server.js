const fastify = require("fastify")();
const handlers = require("./handlers");

fastify.register(require("@fastify/view"), {
  engine: {
    handlebars: require("handlebars")
  }
});

fastify.addHook('onRequest', (request, reply, done) => {
  reply.header("Server", "Fastify");
  done()
})

fastify.get("/json", {
  schema: {
    response: {
      200: {
        type: 'object',
        properties: {
          message: { type: 'string' }
        }
      }
    }
  }
}, (req, reply) => {
  reply
    .header("Content-Type", "application/json")
    .code(200)
    .send({ message: "Hello, World!" });
});

fastify.get("/plaintext", (req, reply) => {
  reply
    .header("Content-Type", "text/plain")
    .code(200)
    .send("Hello, World!");
});

const database = process.env.DATABASE;

if (database) {
  const dbLayer = require(`./db/${database}`);
  const routerHandler = handlers(dbLayer);

  const itemSchema = {
    type: 'object',
    properties: {
      id: { type: 'integer' },
      message: { type: 'string' },
      randomNumber: { type: 'integer' }
    }
  }

  if (database === 'postgres') {
    // postgres return lowercase columns
    itemSchema.properties.randomnumber = { type: 'integer' };
  }

  const singleQuerySchema = {
    schema: {
      response:{
        200: itemSchema
      }
    }
  }

  const multipleQueriesSchema = {
    schema: {
      response:{
        200: {
          type: 'array',
          items: itemSchema
        }
      }
    }
  }

  const updateSchema = {
    schema: {
      response:{
        200: {
          type: 'array',
          items: itemSchema
        }
      }
    }
  }

  fastify.get("/db", singleQuerySchema, routerHandler.singleQuery);
  fastify.get("/queries", multipleQueriesSchema, routerHandler.multipleQueries);
  fastify.get("/fortunes", routerHandler.fortunes);
  fastify.get("/updates", updateSchema, routerHandler.updates);
}

fastify.listen({ port: 8080, host: "0.0.0.0" }, (err, address) => {
  if (err) {
    throw err;
  }

  console.log(
    `Worker started and listening on ${address} ${new Date().toISOString()}`
  );
});
