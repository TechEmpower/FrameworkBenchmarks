const express = require('express');
const app = express();
const bodyParser = require('body-parser');
const port = 8080;

app.use(bodyParser.urlencoded({ extended:false }));
app.use(bodyParser.json());

// Routes

const resolvers = require('./resolver');

require('./routes')(app, resolvers);

app.listen(port, () => {
    console.log(`Listening on localhost:${port}`);
});