const Koa = require('koa');
const Router = require('koa-router');
const hbs = require('koa-hbs');
const bodyParser = require('koa-bodyparser');
const handlebars = require('handlebars');

const MongooseHandler = require('./handlers/mongoose');
const SequelizeHandler = require('./handlers/sequelize');
const SequelizePgHandler = require('./handlers/sequelize-postgres');


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


router.get('/mongoose/db', MongooseHandler.SingleQuery);
router.get('/mongoose/queries', MongooseHandler.MultipleQueries);
router.get('/mongoose/fortunes', MongooseHandler.Fortunes);
router.get('/mongoose/updates', MongooseHandler.Updates);

router.get('/sequelize/db', SequelizeHandler.SingleQuery);
router.get('/sequelize/queries', SequelizeHandler.MultipleQueries);
router.get('/sequelize/fortunes', SequelizeHandler.Fortunes);
router.get('/sequelize/updates', SequelizeHandler.Updates);

router.get('/sequelize-pg/db', SequelizePgHandler.SingleQuery);
router.get('/sequelize-pg/queries', SequelizePgHandler.MultipleQueries);
router.get('/sequelize-pg/fortunes', SequelizePgHandler.Fortunes);
router.get('/sequelize-pg/updates', SequelizePgHandler.Updates);

app.use(router.routes());
const server = app.listen(8080);
console.log('Worker started and listening on http://0.0.0.0:8080 ' 
  + new Date().toISOString(" "));
