var Handlebars = require('handlebars');

var GREETING = "Hello, World";
var HELLO_OBJ = { message: GREETING }

module.exports = {
  randomTfbNumber: function() {
    return Math.floor(Math.random() * 10000) + 1;
  },

  fillArray: function(value, len) {
    var filled = [];

    for (var i = 0; i < len; i++) {
      filled.push(value);
    }
    return filled;
  },

  getQueries: function(req) {
    var queries = ~~(req.query.queries) || 1;
    queries = Math.min(Math.max(queries, 1), 500);
    return queries;
  },

  ADDITIONAL_FORTUNE: {
    id: 0,
    message: 'Additional fortune added at request time.'
  }

}