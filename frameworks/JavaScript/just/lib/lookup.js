const { create, parse } = require('dns.js')
const { udp, net } = just
const { loop } = just.factory
const { readFile } = require('fs')

const dnsServer = just.env().DNS_SERVER || '127.0.0.11'

function parseLine (line) {
  const parts = line.split(/\s+/)
  const [address, ...hosts] = parts
  return { address, hosts }
}

const rxipv4 = /\d+\.\d+\.\d+\.\d+/
const rxComment = /(\s+)?#.+/
const rxName = /nameserver\s+(.+)/

function readHosts () {
  const hosts = readFile('/etc/hosts')
  just.print(`/etc/hosts:\n${hosts}`)
  const lines = hosts.split('\n').filter(line => line.trim())
  const ipv4 = {}
  const ipv6 = {}
  for (const line of lines) {
    if (line.match(rxComment)) continue
    const { address, hosts } = parseLine(line)
    if (address.match(rxipv4)) {
      for (const host of hosts) {
        ipv4[host] = address
      }
    } else {
      for (const host of hosts) {
        ipv6[host] = address
      }
    }
  }
  return { ipv4, ipv6 }
}

function lookupHosts (hostname) {
  const { ipv4 } = readHosts()
  return ipv4[hostname]
}

function readResolv () {
  const resolv = readFile('/etc/resolv.conf')
  just.print(`/etc/resolv.conf:\n${resolv}`)
  const lines = resolv.split('\n').filter(line => line.trim())
  const results = []
  for (const line of lines) {
    const match = line.match(rxName)
    if (match && match.length > 1) {
      const [, ip] = match
      if (ip.match(rxipv4)) {
        results.push(ip)
      }
    }
  }
  return results
}

function lookup (query = 'www.google.com', onRecord = () => {}, address = dnsServer, port = 53, buf = new ArrayBuffer(65536)) {
  const ip = lookupHosts(query)
  if (ip) {
    just.print(`found ${ip} for ${query} in /etc/hosts`)
    onRecord(null, ip)
    return
  }
  const ips = readResolv()
  if (ips.length) {
    address = ips[0]
    just.print(`dns server ${address} found in /etc/resolv.conf`)
  }
  const fd = net.socket(net.AF_INET, net.SOCK_DGRAM | net.SOCK_NONBLOCK, 0)
  net.bind(fd, address, port)
  loop.add(fd, (fd, event) => {
    just.clearTimeout(timer)
    const answer = []
    const len = udp.recvmsg(fd, buf, answer)
    if (len <= 0) {
      onRecord(new Error('Bad Message Length'))
      return
    }
    const message = parse(buf, len)
    if (!message.answer.length) {
      onRecord(new Error(`Address Not Found for ${query}`))
      return
    }
    const { ip } = message.answer[0]
    const result = `${ip[0]}.${ip[1]}.${ip[2]}.${ip[3]}`
    just.print(`got ${result} for ${query} from ${address}`)
    loop.remove(fd)
    net.close(fd)
    onRecord(null, result)
  })
  const timer = just.setTimeout(() => {
    onRecord(new Error(`Request timed out for ${query} at ${address}`))
    loop.remove(fd)
    net.close(fd)
  }, 1000)
  const len = create(query, buf, 1)
  const rc = udp.sendmsg(fd, buf, address, port, len)
}

module.exports = { lookup }
