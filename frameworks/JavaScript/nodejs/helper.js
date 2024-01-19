const Handlebars = require('handlebars');
const { sjs, attr } = require("slow-json-stringify");

const GREETING = "Hello, World!";

const headerTypes = {
  plain: 'text/plain',
  json:  'application/json',
  html:  'text/html; charset=UTF-8'
};

function sortByMessage(arr) {
  const n = arr.length;
  for (let i = 1; i < n; i++) {
    const c = arr[i];
    let j = i - 1;
    while (j > -1 && c.message < arr[j].message) {
      arr[j + 1] = arr[j];
      j--;
    }
    arr[j + 1] = c;
  }
  return arr;
}

function generateRandomNumber(){
  return Math.ceil(Math.random() * 10000);
}

const jsonSerializer = sjs({ message: attr("string") });
const worldObjectSerializer = sjs({ id: attr("number"), randomnumber: attr("number")});
const escapeHTMLRules = { '&': '&#38;', '<': '&#60;', '>': '&#62;', '"': '&#34;', "'": '&#39;', '/': '&#47;' }

const unsafeHTMLMatcher = /[&<>"'\/]/g

function escapeHtmlFromText(text) {
  if (unsafeHTMLMatcher.test(text) === false) return text;
  return text.replace(unsafeHTMLMatcher, function (m) { return escapeHTMLRules[m] || m; });
}

function writeResponse(res, text, type = headerTypes["json"]) {
  res.writeHead(200, {
    "content-type": type,
    server: "Node",
  });
  res.end(text);
}

const self = (module.exports = {
  sortByMessage,
  generateRandomNumber,
  jsonSerializer,
  worldObjectSerializer,
  escapeHtmlFromText,
  writeResponse,
  headerTypes,

  additionalFortune: () => ({
    id: 0,
    message: "Additional fortune added at request time.",
  }),

  fortunesTemplate: Handlebars.compile(
    [
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
      "</html>",
    ].join("")
  ),

  randomTfbNumber: () => Math.floor(Math.random() * 10000) + 1,

  fillArray: (value, len) => {
    const arr = [];
    for (let i = 0; i < len; i++) {
      arr.push(value);
    }
    return arr;
  },

  addTfbHeaders: (res, headerType) => {
    res.setHeader('Server', 'Node');
    res.setHeader('Content-Type', headerTypes[headerType]);
      },

  responses: {
    jsonSerialization: (req, res) => {
      writeResponse(res, jsonSerializer({ message: GREETING }));
    },

    plaintext: (req, res) => {
      writeResponse(res, GREETING, headerTypes['plain']);
    },

    routeNotImplemented: (req, res) => {
      res.writeHead(501, { "Content-Type": "text/plain; charset=UTF-8" });
      const reason = {
        reason: "`" + req.url + "` is not an implemented route",
      };
      res.end(JSON.stringify(reason));
    },
  },
});
