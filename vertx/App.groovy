// Our application config

def persistorConf = [
  address: 'hello.persistor',
  db_name: 'hello_world',
  host: '127.0.0.1'
]

def permitted =
[
  // Allow calls to get static album data from the persistor
  //[
  //  'address' : 'hello.persistor',
  //  'match' : [
  //    'action' : 'find',
  //    'collection' : 'users'
  //  ]
  //]
  [
    'address' : 'hello.persistor'
  ]
]

container.with {

  // Deploy the busmods
  // deployModule('vertx.mongo-persistor-v1.2.1', persistorConf, 8)

  // Start the web server

  deployVerticle('WebServer', ['permitted': permitted], 8)
}
