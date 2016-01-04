var Handlebars = require('handlebars');

var GREETING = "Hello, World!";

var self = module.exports = {

  ADDITIONAL_FORTUNE: {
    id: 0,
    message: 'Additional fortune added at request time.'
  },

  fortunesTemplate: Handlebars.compile([
    "<!DOCTYPE html>",
    "<html>",
    "<head><title>Fortunes</title></head>",
    "<body>",
    "<table>",
      "<tr>",
        "<th>id</th>",
        "<th>message</th>",
      "</tr>",
      "{{#fortunes}}",
      "<tr>",
        "<td>{{id}}</td>",
        "<td>{{message}}</td>",
      "</tr>",
      "{{/fortunes}}",
    "</table>",
    "</body>",
    "</html>"
  ].join('')),

  randomTfbNumber: function () {
    return Math.floor(Math.random() * 10000) + 1;
  },

  fillArray: function(value, len) {
    var arr = [];
    for (var i = 0; i < len; i++) {
      arr.push(value);
    }
    return arr;
  },

  addTfbHeaders: function (res, headerType) {
    var headers = {
      'Server': 'Node'
    }

    if (headerType === 'plain') {
      headers['Content-Type'] = 'text/plain; charset=UTF-8';
    } else if (headerType === 'json') {
      headers['Content-Type'] = 'application/json';
    } else if (headerType === 'html') {
      headers['Content-Type'] = 'text/html; charset=UTF-8';
    }

    res.writeHead(200, headers);
  },

  responses: {

    jsonSerialization: function (req, res) {
      var HELLO_OBJ = { message: GREETING }
      self.addTfbHeaders(res, 'json');
      res.end(JSON.stringify(HELLO_OBJ));
    },

    plaintext: function (req, res) {
      self.addTfbHeaders(res, 'plain');
      res.end(GREETING);
    },

    routeNotImplemented: function (req, res) {
      res.writeHead(501, {'Content-Type': 'text/plain; charset=UTF-8'});
      var reason = { reason: "`" + req.url + "` is not an implemented route" };
      res.end(JSON.stringify(reason));
    }

  }

};
