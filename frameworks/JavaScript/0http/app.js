const cero = require('0http')
const low = require('0http/lib/server/low')

const { router, server } = cero({
  server: low()
})

router.on('GET', '/json', (req, res) => {
  res.setHeader('server', '0http')
  res.setHeader('content-type', 'application/json')

  res.end(JSON.stringify({ message: 'Hello, World!' }))
})

router.on('GET', '/plaintext', (req, res) => {
  res.setHeader('server', '0http')
  res.setHeader('content-type', 'text/plain')

  res.end('Hello, World!')
})

server.start(8080, socket => {})