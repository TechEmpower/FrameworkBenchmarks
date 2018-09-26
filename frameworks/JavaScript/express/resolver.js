const Sequelize = require('sequelize');

// MySQL

const connection = {
    database: 'hello_world',
    username: 'benchmarkdbuser',
    password: 'benchmarkdbpass',
    host: 'tfb-database',
    dialect: 'mysql'
}

const sequelize = new Sequelize(
    connection.database,
    connection.username,
    connection.password, {
        host: connection.host,
        dialect: connection.dialect,
        logging: false
    },
);

const World = sequelize.define('world', {
    id: {
        autoIncrement: true,
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

const Fortune = sequelize.define('fortune', {
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

const randomizeNum = () => {

    let randomInt = Math.floor(Math.random() * 10000) + 1
    return randomInt;
}

async function arrayOfRandomWorlds(totalWorldToReturn) {

    var totalIterations = sanititizeTotal(totalWorldToReturn);
    var arr = [];

    return new Promise(async (resolve, reject) => {
        for(var i = 0; i < totalIterations; i++) {
            let world = await World.findById(randomizeNum());
            arr.push(world);
        }
        if(arr.length == totalIterations) {
            resolve(arr);
        }
    });
};

async function updateRandomWorlds(totalToUpdate) {

    const total = sanititizeTotal(totalToUpdate);
    var arr = [];

    return new Promise(async (resolve, reject) => {
        for(var i = 0; i < total; i++) {

            const world = await World.findById(randomizeNum());
            world.updateAttributes({
                randomNumber: randomizeNum()
            })
            arr.push(world);
        }
        if(arr.length == total) {
            resolve(arr);
        }
    });
};

const sanititizeTotal = (total) => {

    var totalIterations;

    if (!total || typeof(total) != 'number') {
        totalIterations = 1;
    } else if(total < 501 && total > 0) {
        totalIterations = total;
    } else if (total > 500) {
        totalIterations = 500;
    } else {
        totalIterations = 1;
    }
    return totalIterations;
};

const sayHello = () => {

    var helloWorld = new Object;
    helloWorld.message = "Hello, World!";

    return JSON.stringify(helloWorld);
};

module.exports = {
    Query: {
        helloWorld: () => sayHello(),
        getAllWorlds: async() => await World.findAll(),
        singleDatabaseQuery: async() => await World.findById(randomizeNum()),
        multipleDatabaseQueries: async(parent, args) => await arrayOfRandomWorlds(args.total),
        getWorldById: async(parent, args) => await World.findById(args.id),
        getAllFortunes: async() => await Fortune.findAll(),
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