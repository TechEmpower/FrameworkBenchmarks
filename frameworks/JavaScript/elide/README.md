# [Elide](https://github.com/elide-dev/elide) - A fast polyglot runtime

## Description

Elide is a runtime which can execute **JavaScript**, **Ruby**, **Python**, and others, all in one package.

Elide is powered by [GraalVM](https://graalvm.org), which enables polyglot software design. Code units can interoperate from any supported language.

The test script embedded for this benchmark uses Elide's built-in HTTP intrinsic from JavaScript:

```javascript
// access the built-in HTTP server engine
const app = Elide.http;

// register basic handler
app.router.handle("GET", "/plaintext", (request, response) => {
  // respond using the captured path variables
  response.send(200, "Hello, world!");
});

// register a route handler
app.router.handle("GET", "/json", (request, response, context) => {
  // respond using the captured path variables
  response.send(200, JSON.stringify({ message: "Hello, world!" }));
});

// configure the server binding options
app.config.port = 3000;

// receive a callback when the server starts
app.config.onBind(() => {
  console.log(`Server listening at "http://localhost:${app.config.port}"! 🚀`);
});

// start the server
app.start();
```

- [Elide Docs](https://docs.elide.dev)

## Test URLs

### Test 1: JSON Encoding

    http://localhost:3000/json

### Test 2: Plaintext

    http://localhost:3000/plaintext
