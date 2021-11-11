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

async function arrayOfRandomWorlds(totalWorldsToReturn) {

    var totalIterations = helper.sanititizeTotal(totalWorldsToReturn);
    var arr = [];

    return new Promise(async(resolve, reject) => {

        for(var i = 0; i < totalIterations; i++) {
            arr.push(await getRandomWorld());
        }
        
        if(arr.length == totalIterations) {
            resolve(arr);
        }
    });
};

async function updateRandomWorlds(totalToUpdate) {

    const total = helper.sanititizeTotal(totalToUpdate);
    var arr = [];

    return new Promise(async(resolve, reject) => {
        for(var i = 0; i < total; i++) {
            arr.push(await updateRandomWorld());
        }

        if(arr.length === total) resolve(arr)
    });
};

const getRandomWorld = async () => {

    let world = await db.one(`select * from World where id = ${helper.randomizeNum()}`, [true])
    return {"id": world.id, "randomNumber": world.randomnumber};
};

const updateRandomWorld = async () => {

    let world = await db.oneOrNone(`update world set randomNumber = ${helper.randomizeNum()} where id = ${helper.randomizeNum()} returning id, randomNumber`, [true])
    return {"id": world.id, "randomNumber": world.randomnumber};
};

const getAllFortunes = async () => {

    return await db.many('select * from fortune', [true]);
};

module.exports = {
    Query: {
        singleDatabaseQuery: async() => await getRandomWorld(),
        multipleDatabaseQueries: async(parent, args) => await arrayOfRandomWorlds(args.total),
        getAllFortunes: async() => await getAllFortunes(),
        getRandomAndUpdate: async(parent, args) => await updateRandomWorlds(args.total)
    },
    Mutation: {
        createWorld: async(parent, args) => {
            let randInt = Math.floor(Math.random() * 1000) + 1;
            return await World.create({ id: null, randomNumber: randInt });
        },
        updateWorld: async(parent, args) => {
            return await World.update({id: args.id, randomNumber: args.randomNumber});
        }
    }
}