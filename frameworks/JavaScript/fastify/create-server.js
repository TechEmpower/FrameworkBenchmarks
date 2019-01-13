const fastify = require("fastify")();
const handlers = require("./handlers");

fastify.register(require("point-of-view"), {
  engine: {
    ejs: require("handlebars")
  },
  templates: __dirname + "/views"
});

fastify.use((req, reply, next) => {
  reply.setHeader("Server", "Fastify");

  next();
});

fastify.get("/json", (req, reply) => {
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

  fastify.get("/db", routerHandler.singleQuery);
  fastify.get("/queries", routerHandler.multipleQueries);
  fastify.get("/fortunes", routerHandler.fortunes);
  fastify.get("/updates", routerHandler.updates);
}

fastify.listen(8080, "0.0.0.0", err => {
  if (err) {
    throw err;
  }

  console.log(
    `Worker started and listening on http://0.0.0.0:8080 ${new Date().toISOString()}`
  );
});
