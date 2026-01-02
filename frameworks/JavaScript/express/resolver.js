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

function arrayOfRandomWorlds(totalWorldToReturn) {

    const totalIterations = helper.sanititizeTotal(totalWorldToReturn);
    const arr = [];

    for(let i = 0; i < totalIterations; i++) {
        arr.push(World.findByPk(helper.randomizeNum()));
    }

    return Promise.all(arr);
};

async function updateRandomWorlds(totalToUpdate) {

    const total = helper.sanititizeTotal(totalToUpdate);
    const arr = [];

    for(let i = 0; i < total; i++) {
        arr.push(World.findByPk(helper.randomizeNum()));
    }

    const results = await Promise.all(arr);

    const updates = [];
    for(const world of results){
        updates.push(world.updateAttributes({
            randomNumber: helper.randomizeNum()
        }));
    }

    await Promise.all(updates);

    return results;
}

const sayHello = () => {
    return JSON.stringify({ message:"Hello, World!" });
};

module.exports = {
    Query: {
        helloWorld: () => sayHello(),
        getAllWorlds: () => World.findAll(),
        singleDatabaseQuery: () => World.findByPk(helper.randomizeNum()),
        multipleDatabaseQueries: (parent, args) => arrayOfRandomWorlds(args.total),
        getWorldById: (parent, args) => World.findByPk(args.id),
        getAllFortunes: () => Fortune.findAll(),
        getRandomAndUpdate: (parent, args) => updateRandomWorlds(args.total)
    },
    Mutation: {
        createWorld: (parent, args) => {
            return World.create({ id: null, randomNumber: Math.floor(Math.random() * 1000) + 1 });
        },
        updateWorld: (parent, args) => {
            return World.update({id: args.id, randomNumber: args.randomNumber});
        }
    }
}
