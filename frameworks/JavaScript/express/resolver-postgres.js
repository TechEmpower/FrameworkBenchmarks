const pgp = require('pg-promise')();
const helper = require('./helper');

// MySQL

const connection = {
    db: 'hello_world',
    username: 'benchmarkdbuser',
    password: 'benchmarkdbpass',
    host: 'tfb-database',
    dialect: 'postgres'
}

const db = pgp(`postgres://${connection.username}:${connection.password}@${connection.host}:5432/${connection.db}`);

function arrayOfRandomWorlds(totalWorldsToReturn) {

    const totalIterations = helper.sanititizeTotal(totalWorldsToReturn);
    const arr = [];

    for(let i = 0; i < totalIterations; i++) {
        arr.push(getRandomWorld());
    }

    return Promise.all(arr);
};

function updateRandomWorlds(totalToUpdate) {

    const total = helper.sanititizeTotal(totalToUpdate);
    const arr = [];

    for(let i = 0; i < total; i++) {
        arr.push(updateRandomWorld());
    }

    return Promise.all(arr);
};

const getRandomWorld = async () => {

    let world = await db.one(`select * from World where id = ${helper.randomizeNum()}`, [true])
    return {"id": world.id, "randomNumber": world.randomnumber};
};

const updateRandomWorld = async () => {

    let world = await db.oneOrNone(`update world set randomNumber = ${helper.randomizeNum()} where id = ${helper.randomizeNum()} returning id, randomNumber`, [true])
    return {"id": world.id, "randomNumber": world.randomnumber};
};

const getAllFortunes =  () => {
    return db.many('select * from fortune', [true]);
};

module.exports = {
    Query: {
        singleDatabaseQuery: () => getRandomWorld(),
        multipleDatabaseQueries: (parent, args) => arrayOfRandomWorlds(args.total),
        getAllFortunes: () => getAllFortunes(),
        getRandomAndUpdate: (parent, args) => updateRandomWorlds(args.total)
    },
    Mutation: {
        createWorld: (parent, args) => {
            let randInt = Math.floor(Math.random() * 1000) + 1;
            return World.create({ id: null, randomNumber: randInt });
        },
        updateWorld: (parent, args) => {
            return World.update({id: args.id, randomNumber: args.randomNumber});
        }
    }
}