const { sjs, attr } = require('slow-json-stringify');
const jsonSerializer = sjs({ message: attr("string")});

module.exports = {
    GET: ({ res }) => {
        res.headers['server'] = 'spliffy';
        res.headers['content-type'] = 'application/json';
        return jsonSerializer({ message: 'Hello, World!' });
    }
}