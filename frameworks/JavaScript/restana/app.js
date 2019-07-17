const cluster = require('cluster')
const cpus = require('os').cpus()

const service = require('restana')()

service.get('/json', (req, res) => {
  res.setHeader('server', 'restana')
  res.setHeader('content-type', 'application/json')
  
  res.end(JSON.stringify({ message: 'Hello, World!' }))
})

service.get('/plaintext', (req, res) => {
  res.setHeader('server', 'restana')
  res.setHeader('content-type', 'text/plain')

  res.end('Hello, World!')
})

if (cluster.isMaster) {
  cpus.forEach(() => cluster.fork())
} else {
  service.start(8080)
}
