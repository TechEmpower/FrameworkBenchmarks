const Koa = require('koa');
const Router = require('koa-router');
const hbs = require('koa-hbs');
const bodyParser = require('koa-bodyparser');
const handlebars = require('handlebars');
const handler = require('./handlers/handler');

const app = new Koa();
const router = new Router();

app
  .use(async (ctx, next) => {
    ctx.set('Server', 'Koa');
    await next();
  });

router
  .get('/json', (ctx) => {
    ctx.body = {message: 'Hello, World!'};
  })
  .get('/plaintext', (ctx) => {
    ctx.body = 'Hello, World!';
  });

const handlerName = process.env.NODE_HANDLER;

if (handlerName) {
  const dbLayer = require(`./handlers/${handlerName}`);

  const routerHandler = handler(dbLayer);

  router
    .use(bodyParser())
    .use(hbs.middleware({
      handlebars: handlebars,
      viewPath: __dirname + '/views'
    }))
    .get('/db', routerHandler.SingleQuery)
    .get('/queries', routerHandler.MultipleQueries)
    .get('/fortunes', routerHandler.Fortunes)
    .get('/updates', routerHandler.Updates);
}

app.use(router.routes());

app.listen(8080);
console.log(`Worker started and listening on http://0.0.0.0:8080 ${new Date().toISOString()}`);
