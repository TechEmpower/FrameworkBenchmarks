import vibe.appmain;
import vibe.d;
import std.random;

const worldSize = 10000;
const fortunesSize = 100;
const mongoUrl = "mongodb://127.0.0.1/";

MongoClient mongo;
MongoCollection worldCollection;
MongoCollection fortunesCollection;

shared static this()
{
  mongo = connectMongoDB( mongoUrl );
  worldCollection = mongo.getCollection( "hello_world.World" );
  fortunesCollection = mongo.getCollection( "hello_world.Fortunes" );

  auto router = new URLRouter;
  router.get("/plaintext", &plaintext);
  router.get("/json", &json);
  router.get("/db", &db);
  router.get("/queries", &queries);
  router.get("/generate-world", &generateWorld);
  router.get("/generate-fortunes", &generateFortunes);
  router.get("/", staticTemplate!"index.dt");

  auto settings = new HTTPServerSettings;
  settings.port = 8080;

  listenHTTP(settings, router);
}

void json(HTTPServerRequest req, HTTPServerResponse res)
{
  auto helloWorld  = Json([
    "message" : *new Json( "Hello, World!" )
  ]);
  res.writeJsonBody( helloWorld );
}

void generateWorld(HTTPServerRequest req, HTTPServerResponse res)
{
  try {
    worldCollection.drop();
  } catch( Exception error ) {}
  for( auto i = 0 ; i < worldSize ; ++i ) {
    worldCollection.insert([
      "_id": i + 1,
      "randomNumber": uniform( 0 , worldSize )
    ]);
  }
  res.writeBody( "Generated" );
}

void generateFortunes(HTTPServerRequest req, HTTPServerResponse res)
{
  try {
    fortunesCollection.drop();
  } catch( Exception error ) {}
  for( uint i = 0 ; i < worldSize ; ++i ) {
    fortunesCollection.insert([
      "_id": new Bson( i + 1 ),
      "message": new Bson( to!string( uniform( 0 , fortunesSize ) ) )
    ]);
  }
  res.writeBody( "Generated" );
}

void db(HTTPServerRequest req, HTTPServerResponse res)
{
  auto data = worldCollection.findOne([
    "_id": uniform( 1 , worldSize + 1 )
  ]); 
  res.writeJsonBody( data );
}

void queries(HTTPServerRequest req, HTTPServerResponse res)
{
  auto count = 1;
  try {
    count = to!uint( req.query["queries"] );
    if( !count ) {
      count = 1;
    } else if( count > 500 ) {
      count = 500;
    }
  } catch( Exception error ) { }
  
  auto data = new Bson[ count ];
  for( uint i = 0 ; i < count ; ++i ) {
    data[i] = worldCollection.findOne([
      "_id": uniform( 1 , worldSize + 1 )
    ]);
  }
  res.writeJsonBody( data );
}

void plaintext(HTTPServerRequest req, HTTPServerResponse res)
{
  res.writeBody("Hello, World!");
}
