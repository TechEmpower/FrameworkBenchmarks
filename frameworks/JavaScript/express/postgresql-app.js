const express = require('express'),
  app = express(),
  bodyParser = require('body-parser'),
  pgp = require('pg-promise')(),
  helper = require('./helper');

const connection = {
    db: 'hello_world',
    username: 'benchmarkdbuser',
    password: 'benchmarkdbpass',
    host: 'tfb-database',
    dialect: 'postgres'
}

const db = pgp(`postgres://${connection.username}:${connection.password}@${connection.host}:5432/${connection.db}`);

app.set('view engine', 'pug');
app.set('views', __dirname + '/views');
app.use(bodyParser.urlencoded({ extended: true }));
app.use(bodyParser.json());

// Set headers for all routes
app.use((req, res, next) => {
    res.setHeader("Server", "Express");
    return next();
});

// Routes
app.get('/db', async (req, res) => {

    let world = await getRandomWorld();

    res.setHeader("Content-Type", "application/json");
    res.json(world);
});

app.get('/queries', async (req, res) => {

    const results = [],
        queries = Math.min(parseInt(req.query.queries) || 1, 500);

    for (let i = 0; i < queries; i++) {
        
        results.push(await getRandomWorld());
    }

    res.json(results)
});

app.get('/fortunes', async (req, res) => {

    let fortunes = await getAllFortunes()
    const newFortune = { id: 0, message: "Additional fortune added at request time." };
    fortunes.push(newFortune);
    fortunes.sort((a, b) => (a.message < b.message) ? -1 : 1);

    res.render('fortunes/index', { fortunes: fortunes });
});

app.get('/updates', async (req, res) => {
    const results = [],
        queries = Math.min(parseInt(req.query.queries) || 1, 500);

    for (let i = 1; i <= queries; i++) {

        results.push(await updateRandomWorld())
    }

    res.json(results);
});

const getRandomWorld = async () => {

    return await db.one(`select * from world where id = ${helper.randomizeNum()}`, [true])
};

const updateRandomWorld = async () => {

    return await db.oneOrNone(`update world set randomNumber = ${helper.randomizeNum()} where id = ${helper.randomizeNum()} returning id, randomNumber`, [true])
};

const getAllFortunes = async () => {

    return await db.many('select * from fortune', [true]);
};

app.listen(8080, () => {
    console.log('listening on port 8080');
});