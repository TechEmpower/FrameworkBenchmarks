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
  response.header("Content-Type", "application/json");
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
