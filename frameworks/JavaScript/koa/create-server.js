const Koa = require('koa');
const Router = require('koa-router');
const hbs = require('koa-hbs');
const bodyParser = require('koa-bodyparser');
const handlebars = require('handlebars');

const Handler = require(`./handlers/${process.env.NODE_HANDLER}`);

const app = new Koa();
const router = new Router();
app.use(bodyParser());
app.use(hbs.middleware({
  handlebars: handlebars,
  viewPath: __dirname + '/views'
}));

function JsonSerialization(ctx, next) {
  ctx.set('Server', 'Koa');
  ctx.type = 'application/json';
  ctx.body = { message: 'Hello, World!' };
  return next();
}

function Plaintext(ctx, next) {
  ctx.set('Server', 'Koa');
  ctx.type = 'text/plain';
  ctx.body = 'Hello, World!';
  return next();
}

router.get('/json', JsonSerialization);
router.get('/plaintext', Plaintext);
router.get('/db', Handler.SingleQuery);
router.get('/queries', Handler.MultipleQueries);
router.get('/fortunes', Handler.Fortunes);
router.get('/updates', Handler.Updates);


app.use(router.routes());
const server = app.listen(8080);
console.log('Worker started and listening on http://0.0.0.0:8080 ' 
  + new Date().toISOString(" "));
