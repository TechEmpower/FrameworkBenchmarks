const cluster = require('cluster')
const cpus = require('os').cpus()

const service = require('restana')()

service.get('/json', (req, res) => {
  res.end(JSON.stringify({ message: 'Hello, World!' }))
})

service.get('/plaintext', (req, res) => {
  res.end('Hello, World!')
})

if (cluster.isMaster) {
  cpus.forEach(() => cluster.fork())
} else {
  service.start(8080)
}
