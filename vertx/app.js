var container = require('vertx/container')

var persistorConf = {
  address: 'hello.persistor',
  db_name: 'hello_world',
  host: 'localhost'
}

container.deployModule('io.vertx~mod-mongo-persistor~2.0.0-final', persistorConf, function (err, dep_id) {
  if (!err) {
    container.deployVerticle('WebServer.java', 8);
  } else {
    err.printStackTrace();
  }
});
