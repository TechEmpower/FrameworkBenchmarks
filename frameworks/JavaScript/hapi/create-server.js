/**
 * Currently commenting out redis caching as there is no
 * working implementation for the benchmark suite.
 */

const Hapi = require('@hapi/hapi');
const Vision = require('@hapi/vision');

const options = {
  port: process.env.PORT || 8080, 
  host: '0.0.0.0',
  compression: false
};

const server = new Hapi.server(options);

const provision = async () => {

  await server.register({
    plugin: Vision,
    options: {
      engines: { html: require('handlebars') },
      path: __dirname + '/views/'
    }
  })
  
  const Handler = require(`./handlers/${process.env.NODE_HANDLER}`);

  // Makes routing simpler as tfb routes are all GET's
  // We also don't use the nifty route features that Hapi has
  // to offer such as attaching a validator
  const Route = (path, handler) =>
    server.route({ method: 'GET', path, handler });
  
  const JsonSerialization = (request, h) =>
    h.response({ message: 'Hello, World!' }).header('Server', 'hapi');
  
  const Plaintext = (request, h) =>
    h.response('Hello, World!')
      .header('Server', 'hapi')
      .header('Content-Type', 'text/plain');
  
  
  Route('/json', JsonSerialization);
  Route('/plaintext', Plaintext);
  
  Route('/db', Handler.SingleQuery);
  Route('/queries', Handler.MultipleQueries);
  Route('/fortunes', Handler.Fortunes);
  Route('/updates', Handler.Updates);

  await server.start();

  console.log('Hapi worker started and listening on ' + server.info.uri + " "
    + new Date().toISOString(" "));
}

provision()


