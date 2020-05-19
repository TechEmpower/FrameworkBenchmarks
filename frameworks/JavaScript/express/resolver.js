const Sequelize = require('sequelize');
const helper = require('./helper');

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

async function arrayOfRandomWorlds(totalWorldToReturn) {

    var totalIterations = helper.sanititizeTotal(totalWorldToReturn);
    var arr = [];

    return new Promise(async (resolve, reject) => {
        for(var i = 0; i < totalIterations; i++) {
            let world = await World.findByPk(helper.randomizeNum());
            arr.push(world);
        }
        if(arr.length == totalIterations) {
            resolve(arr);
        }
    });
};

async function updateRandomWorlds(totalToUpdate) {

    const total = helper.sanititizeTotal(totalToUpdate);
    var arr = [];

    return new Promise(async (resolve, reject) => {
        for(var i = 0; i < total; i++) {

            const world = await World.findByPk(helper.randomizeNum());
            world.updateAttributes({
                randomNumber: helper.randomizeNum()
            })
            arr.push(world);
        }
        if(arr.length == total) {
            resolve(arr);
        }
    });
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
        singleDatabaseQuery: async() => await World.findByPk(helper.randomizeNum()),
        multipleDatabaseQueries: async(parent, args) => await arrayOfRandomWorlds(args.total),
        getWorldById: async(parent, args) => await World.findByPk(args.id),
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
