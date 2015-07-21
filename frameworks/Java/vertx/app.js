var container = require('vertx/container')

var persistorConf = {
  address: 'hello.persistor',
  db_name: 'hello_world',
  host: '127.0.0.1',
  pool_size: 100
}

container.deployModule('io.vertx~mod-mongo-persistor~2.1.1', persistorConf, function (err, dep_id) {
  if (!err) {
    container.deployVerticle('WebServer.java', java.lang.Runtime.getRuntime().availableProcessors() * 2);
  } else {
    err.printStackTrace();
  }
});
