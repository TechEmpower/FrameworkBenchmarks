const express = require('express');
const mongoose = require('mongoose');
const app = express();
const bodyParser = require('body-parser');
const port = 8080;

app.use(bodyParser.urlencoded({ extended:false }));
app.use(bodyParser.json());

mongoose.Promise = global.Promise;

mongoose.connect('mongodb://tfb-database/hello_world').then(() => {
    console.log('connected to mongo tfb-database hello_world');
}).catch((err) => {
    console.log('Failed connection attempt to Mongo: ', err);
});

const resolvers = require('./resolver-mongo');

// Routes

require('/routes.js')(app, resolvers);

app.listen(port, () => {
    console.log(`Listening on localhost:${port}`);
});