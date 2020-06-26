
/**
 * Module dependencies.
 */

const cluster = require('cluster'),
  numCPUs = require('os').cpus().length,
  express = require('express'),
  Sequelize = require('sequelize');

// Middleware
const bodyParser = require('body-parser');

const sequelize = new Sequelize('hello_world', 'benchmarkdbuser', 'benchmarkdbpass', {
  host: 'tfb-database',
  dialect: 'mysql',
  logging: false
});

const World = sequelize.define('world', {
  id: {
    type: 'Sequelize.INTEGER',
    primaryKey: true
  },
  randomNumber: {
    type: 'Sequelize.INTEGER'
  }
}, {
    timestamps: false,
    freezeTableName: true
  });

const Fortune = sequelize.define('Fortune', {
  id: {
    type: 'Sequelize.INTEGER',
    primaryKey: true
  },
  message: {
    type: 'Sequelize.STRING'
  }
}, {
    timestamps: false,
    freezeTableName: true
  });

if (cluster.isMaster) {
  // Fork workers.
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', (worker, code, signal) =>
    console.log('worker ' + worker.pid + ' died'));
} else {
  const app = module.exports = express();

  // Configuration
  // https://github.com/expressjs/method-override#custom-logic
  app.use(bodyParser.urlencoded({ extended: true }));

  // Set headers for all routes
  app.use((req, res, next) => {
    res.setHeader("Server", "Express");
    return next();
  });

  app.set('view engine', 'pug');
  app.set('views', __dirname + '/views');

  // Routes
  app.get('/mysql-orm-query', async (req, res) => {
    const results = [],
      queries = Math.min(parseInt(req.query.queries) || 1, 500);

    for (let i = 1; i <= queries; i++) {
      const world = await World.findOne({
        where: {
          id: Math.floor(Math.random() * 10000) + 1
        }
      }
      );
      results.push(world);
    }

    res.setHeader("Content-Type", "application/json");
    res.send(results);
  });

  app.get('/mysql-orm', async (req, res) => {
    const world = await World.findOne({
      where: {
        id: Math.floor(Math.random() * 10000) + 1
      }
    }
    );

    res.setHeader("Content-Type", "application/json");
    res.send(world)
  });

  app.get('/mysql-orm-fortune', (req, res) => {
    Fortune.findAll().then((fortunes) => {
      const newFortune = { id: 0, message: "Additional fortune added at request time." };
      fortunes.push(newFortune);
      fortunes.sort((a, b) => (a.message < b.message) ? -1 : 1);

      res.render('fortunes/index', { fortunes: fortunes });
    });
  });

  app.get('/mysql-orm-update', async (req, res) => {
    const results = [],
      queries = Math.min(parseInt(req.query.queries) || 1, 500);

    for (let i = 1; i <= queries; i++) {
      const world = await World.findOne({
        where: {
          id: ~~(Math.random() * 10000) + 1
        }
      }
      );
      world.randomNumber = ~~(Math.random() * 10000) + 1;
      await world.save();
      results.push(world);
    }

    res.send(results);
  });

  app.listen(8080);
}
