module.exports = `
    type Query {
        getWorldById(id:Int!): World,
        singleDatabaseQuery: World,
        multipleDatabaseQueries(total:Int!): [World],
        getAllWorlds: [World],
        helloWorld: String,
        getAllFortunes: [Fortune],
        getRandomAndUpdate(total:Int!): [World]
    }
    type Mutation {
        createWorld: World!,
        updateWorld(id:Int! randomNumber:String!): World!
    }
    type World {
        id: Int!,
        randomNumber: Int!
    }
    type Fortune {
        id: Int!
        message: String!
    }
`;