/**
 * Module dependencies.
 */
const cluster = require('cluster'),
  numCPUs = require('os').cpus().length,
  express = require('express'),
  helper = require('./helper');

const Sequelize = require('sequelize');
const sequelize = new Sequelize('hello_world', 'benchmarkdbuser', 'benchmarkdbpass', {
  host: 'tfb-database',
  dialect: 'postgres',
  logging: false,
  pool: {
    max: 50,
    min: 0,
    idle: 10000
  }
});

const Worlds = sequelize.define('world', {
  id: {
    type: 'Sequelize.INTEGER',
    primaryKey: true
  },
  randomnumber: { type: 'Sequelize.INTEGER' }
}, {
  timestamps: false,
  freezeTableName: true
});

const Fortunes = sequelize.define('fortune', {
  id: {
    type: 'Sequelize.INTEGER',
    primaryKey: true
  },
  message: { type: 'Sequelize.STRING' }
}, {
  timestamps: false,
  freezeTableName: true
});

const randomWorldPromise = () => {
  return Worlds.findOne({
    where: { id: helper.randomizeNum() }
  });
};

if (cluster.isPrimary) {
  // Fork workers.
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', (worker, code, signal) =>
    console.log('worker ' + worker.pid + ' died'));
} else {
  const app = module.exports = express();

  app.set('x-powered-by', false);
  app.set('etag', false)

  app.set('view engine', 'pug');
  app.set('views', __dirname + '/views');

  // Routes
  app.get('/db', async (req, res) => {
    let world = await randomWorldPromise();

    res.setHeader("Content-Type", "application/json");
    res.setHeader("Server", "Express");
    res.json(world);
  });

  app.get('/queries', async (req, res) => {
    const queries = Math.min(parseInt(req.query.queries) || 1, 500);

    const promisesArray = [];

    for (let i = 0; i < queries; i++) {
      promisesArray.push(randomWorldPromise());
    }

    res.setHeader("Server", "Express");
    res.json(await Promise.all(promisesArray))
  });

  app.get('/fortunes', async (req, res) => {
    let fortunes = await Fortunes.findAll();
    const newFortune = { id: 0, message: "Additional fortune added at request time." };
    fortunes.push(newFortune);
    fortunes.sort((a, b) => (a.message < b.message) ? -1 : 1);

    res.setHeader("Server", "Express");
    res.render('fortunes/index', { fortunes: fortunes });
  });

  app.get('/updates', async (req, res) => {
    const queries = Math.min(parseInt(req.query.queries) || 1, 500);
    const worldPromises = [];

    for (let i = 0; i < queries; i++) {
      worldPromises.push(randomWorldPromise());
    }

    const worldUpdate = async(world) => {
      world.randomnumber = helper.randomizeNum();

      await Worlds.update({
        randomnumber: world.randomnumber
      },
      {
        where: { id: world.id }
      });
      return world;
    };

    const worlds = await Promise.all(worldPromises);
    
    const updates = worlds.map((e) => worldUpdate(e));

    const updated = await Promise.all(updates);
    
    res.setHeader("Server", "Express");
    res.json(updated);
  });

  const server = app.listen(8080, () => {
    console.log('listening on port 8080');
  });
  server.keepAliveTimeout = 0;
}
