const cluster = require('cluster')
const cpus = require('os').cpus()

const cero = require('0http')
const { router, server } = cero()

router.on('GET', '/json', (req, res) => {
  res.end(JSON.stringify({ message: 'Hello, World!' }))
})

router.on('GET', '/plaintext', (req, res) => {
  res.end('Hello, World!')
})

if (cluster.isMaster) {
  cpus.forEach(() => cluster.fork())
} else {
  server.listen(8080)
}
