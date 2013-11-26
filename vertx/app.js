var container = require('vertx/container')

// The server uses the blocking MongoDB API directly so we deploy them as worker verticles
container.deployWorkerVerticle('WebServer.java', 32);


