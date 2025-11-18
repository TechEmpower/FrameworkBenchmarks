const cero = require('0http')
const { router, server } = cero()

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

server.listen(8080)