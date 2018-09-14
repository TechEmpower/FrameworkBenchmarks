const express = require('express');
const app = express();
const graphqlHTTP = require('express-graphql');
const escape = require('escape-html');
const dateFormat = require('dateformat');

const bodyParser = require('body-parser');
const port = 8080;
const { makeExecutableSchema } = require('graphql-tools');

const typeDefs = require('./schema');
const resolvers = require('./resolver');

const schema = makeExecutableSchema({
    typeDefs,
    resolvers
});

app.use(bodyParser.urlencoded({ extended:false }));
app.use(bodyParser.json());

// Routes

app.get('/json', async(req, res) => {
    
    const graphql = await graphqlHTTP((req, res, graphQLParams) => {
        
        graphQLParams.query = "{ helloWorld }";
        graphQLParams.operationName = null;
        graphQLParams.variables = {};

        return graphqlOpts()
    });

    res.real_end = res.end;

    res.end = (data) => {
        
        let toRet;
        const json = JSON.parse(data.toString('utf8'));

        if(json.data.helloWorld) {
            toRet = json.data.helloWorld;
        } else {
            toRet = { helloWorld: null };
        }
        
        setResponseHeaders(res, toRet.length);

        res.real_end(toRet);
    }

    await graphql(req, res);
});

app.get('/db', async (req, res) => {
    
    const graphql = await graphqlHTTP((req, res, graphQLParams) => {
    
        graphQLParams.query = "{ singleDatabaseQuery { id, randomNumber }}";
        graphQLParams.variables = {};
        graphQLParams.operationName = null;
    
        return graphqlOpts();
    });

    singleJsonFormatResponseFromWorld(res, "singleDatabaseQuery");

    await graphql(req, res);
});

app.get('/queries', async (req, res) => {
    
    const graphql = await graphqlHTTP((req, res, graphQLParams) => {
        
        let totalNumOfQueries = ensureQueryIsAnInt(req.query.queries);

        graphQLParams.query = `{ multipleDatabaseQueries(total: ${totalNumOfQueries}) { id, randomNumber }}`
        graphQLParams.variables = {};
        graphQLParams.operationName = null;

        return graphqlOpts();
    });

    multipleJsonFormatResponseFromWorld(res);

    await graphql(req, res);
});

app.get('/fortunes', async (req, res) => {

    const graphql = await graphqlHTTP((req, res, graphQLParams) => {

        graphQLParams.query = "{ getAllFortunes { id, message } }"
        graphQLParams.operationName = null;
        graphQLParams.variables = {};

        return graphqlOpts();
    });

    retrieveAndFormatAllFortunes(res);
    
    await graphql(req, res);
});

app.get('/updates', async (req, res) => {

    const totalNumOfQueries = ensureQueryIsAnInt(req.query.queries);

    const graphql = await graphqlHTTP((req, res, graphQLParams) => {

        graphQLParams.query = `{ getRandomAndUpdate(total: ${totalNumOfQueries}) { id, randomNumber } }`
        graphQLParams.operationName = null;
        graphQLParams.variables = {};

        return graphqlOpts();
    });

    retrieveUpdateAndFormatWorlds(res);

    await graphql(req, res);
});


app.get('/plaintext', (req, res) => {

    const responseTxt = "Hello, World!";

    res.setHeader('Content-Length', responseTxt.length);
    res.setHeader('Server', 'Express-GraphQL-MySQL');
    res.contentType('text/plain');
    res.status(200);

    res.send(responseTxt);
});

// Helper Functions

const ensureQueryIsAnInt = (queryString) => {

    if(queryString === undefined) return 1;

    const possibleInt = parseInt(queryString);
    if(!possibleInt) return 1;

    return possibleInt;
};

const graphqlOpts = (params) => {
    return {
        schema,
        graphiql: false,
        context: params || {}
    }
};

const retrieveUpdateAndFormatWorlds = res => {

    res.real_end = res.end;

    res.end = data => {
        
        let toRet;
        const json = JSON.parse(data.toString('utf8'));

        if(json.data.getRandomAndUpdate) {
            toRet = json.data.getRandomAndUpdate;
        } else {
            toRet = [];
        }

        const jsonToRet = JSON.stringify(toRet);

        setResponseHeaders(res, jsonToRet.length);

        res.real_end(jsonToRet);
    }
};

const retrieveAndFormatAllFortunes = res => {

    res.real_end = res.end;

    res.end = async (data) => {
        let toRet;
        const json = JSON.parse(data.toString('utf8'));

        if(json.data.getAllFortunes) {
            toRet = json.data.getAllFortunes;
        } else {
            toRet = [];
        }

        const newFortune = { "id": 0, "message": "Additional fortune added at request time." };
        toRet.push(newFortune);
        toRet.sort((a, b) => (a.message < b.message) ? -1 : 1);

        const htmlToRet = await spoofHTML(toRet);

        res.contentType('html');
        res.setHeader('Server', 'GraphQL-MySQL');
        res.setHeader('Content-Length', htmlToRet.length + 32);
        res.status(200);

        res.real_end(htmlToRet);
    }
};

const spoofHTML = arr => {

    return new Promise((resolve, reject) => {

        let count = 0;

        let htmlToRet = `<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>`;

        for (let fortune of arr) {

            htmlToRet += `<tr><td>${escape(fortune.id)}</td><td>${escape(fortune.message)}</td></tr>`;
            count++;
        }

        htmlToRet += '</table></body></html>';
    
        if(count == arr.length)resolve(htmlToRet);

    });

};

const multipleJsonFormatResponseFromWorld = (res) => {

    res.real_end = res.end;

    res.end = (data) => {

        let toRet;
        const json = JSON.parse(data.toString('utf8'));

        if(json.error) console.log(json.error);

        if(json.data.multipleDatabaseQueries) {
            toRet = json.data.multipleDatabaseQueries;
        } else {
            toRet = [];
        }

        const jsonToRet = JSON.stringify(toRet);
        
        setResponseHeaders(res, jsonToRet.length);

        res.real_end(jsonToRet);
    }
};

const singleJsonFormatResponseFromWorld = (res, queryName) => {

    res.real_end = res.end;
    
    res.end = (data) => {

        let to_ret;
        const json = JSON.parse(data.toString('utf8'));

        if(json.data[`${queryName}`]) {
            to_ret = {
                id: json.data[`${queryName}`].id,
                randomNumber: json.data[`${queryName}`].randomNumber
            };
        } else {
            to_ret = {id: null, randomNumber: null};
        }

        const jsonToRet = JSON.stringify(to_ret);
        
        setResponseHeaders(res, jsonToRet.length);

        res.real_end(jsonToRet);
    };
};

const setResponseHeaders = (res, jsonLength) => {

    let now = new Date();

    res.status(200);
    res.contentType('application/json', 'charset=UTF-8');
    res.setHeader('Date', dateFormat(now, "ddd, dd mmm yyyy hh:MM:ss Z"));
    res.setHeader('Server', 'GraphQL-Express-MySQL');
    res.setHeader('Content-Length', jsonLength);
};

app.listen(port, () => {
    console.log(`Listening on localhost:${port}`);
});