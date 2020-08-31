const { create, parse } = require('dns.js')
const { udp, net } = just
const { loop } = just.factory

const dnsServer = just.env().DNS_SERVER || '127.0.0.11'

function lookup (query = 'www.google.com', onRecord = () => {}, address = dnsServer, port = 53, buf = new ArrayBuffer(65536)) {
  const fd = net.socket(net.AF_INET, net.SOCK_DGRAM | net.SOCK_NONBLOCK, 0)
  net.bind(fd, address, port)
  loop.add(fd, (fd, event) => {
    const answer = []
    const len = udp.recvmsg(fd, buf, answer)
    const [address, port] = answer
    const message = { length: len, address, port, message: parse(buf, len) }
    loop.remove(fd)
    net.close(fd)
    onRecord(message)
  })
  const len = create(query, buf, 1)
  udp.sendmsg(fd, buf, address, port, len)
}

module.exports = { lookup }
