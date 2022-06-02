const socket = require('@socket')
const dns = require('@dns')
const binary = require('@binary')

const { createSocket } = socket
const { SystemError } = just
const { getIPAddress } = dns
const { AG, AR, AY, AD, AM } = binary.ANSI

const constants = {
  AuthenticationMD5Password: 5,
  fieldTypes: {
    INT4OID: 23,
    VARCHAROID: 1043
  },
  messageTypes: {
    AuthenticationOk: 82,
    ErrorResponse: 69,
    RowDescription: 84,
    CommandComplete: 67,
    ParseComplete: 49,
    BindComplete: 50,
    ReadyForQuery: 90,
    BackendKeyData: 75,
    ParameterStatus: 83,
    ParameterDescription: 116,
    DataRow: 68,
    Prepare: 80,
    Flush: 72,
    Bind: 66,
    StartupMessage: 0,
    PasswordMessage: 112,
    Exec: 69,
    Describe: 68,
    Prepare: 80,
    Bind: 66,
    Sync: 83
  }
}

let connections = 0
const lookup = {}
for (const key of Object.keys(constants.messageTypes)) {
  lookup[constants.messageTypes[key]] = key
}

class PGParser {
  constructor (buf) {
    this.buf = buf
    this.type = 0
    this.code = ''
    this.name = ''
    this.len = 0
    this.want = 0
    this.dv = new DataView(buf)
  }

  parse (bytes) {
    const messages = []
    const { dv } = this
    let off = 0
    while (off < bytes) {
      this.type = dv.getUint8(off)
      if (this.type === 0) { 
        this.len = dv.getUint32(off)
        this.code = ''
      } else {
        this.len = dv.getUint32(off + 1)
        this.code = String.fromCharCode(this.type)
      }
      this.name = lookup[this.type]
      off += this.len + 1
      const { type, code, name, len } = this
      messages.push({ type, code, name, len })
    }
    return messages
  }
}

async function startClient (sock) {
  connections++
  const stats = {
    client: {},
    backend: {}
  }
  sock.edgeTriggered = false
  sock.noDelay = true
  const backend = createSocket()
  backend.edgeTriggered = true
  sock.parser = new PGParser(sock.buffer)
  const ip = await getIPAddress('tfb-proxy-database')
  await backend.connect(ip, 5431)
  backend.noDelay = true
  backend.parser = new PGParser(backend.buffer)
  sock.onReadable = () => {
    const bytes = sock.recv(0)
    if (bytes === 0) {
      sock.close()
      return
    }
    if (bytes > 0) {
      const messages = sock.parser.parse(bytes)
      for (const message of messages) {
        if (stats.client[message.name]) {
          stats.client[message.name]++
        } else {
          stats.client[message.name] = 1
        }
      }
      backend.send(sock.buffer, bytes, 0)
      return
    }
    if (!sock.blocked) {
      just.error(sock.error.stack)
      sock.close()
    }
  }
  sock.onClose = () => {
    connections--
    const { client } = stats
    let status = 'ok'
    //if (!((client.Bind === client.Exec) && (client.Exec === client.Sync))) {
    if (!(client.Exec === client.Sync)) {
      status = 'fail'
    }
    just.print(`${AY}Bind${AD} ${client.Bind || 0} ${AY}Exec${AD} ${client.Exec || 0} ${AY}Sync${AD} ${client.Sync || 0} ${status === 'ok' ? AG : AR }${status}${AD}`)
    //backend.close()
    if (connections === 0) {
      just.print(`${AM}proxy idle${AD}`)
    }
  }
  backend.onReadable = () => {
    const bytes = backend.recv(0)
    if (bytes === 0) {
      backend.close()
      return
    }
    if (bytes > 0) {
      const messages = backend.parser.parse(bytes)
      for (const message of messages) {
        if (stats.backend[message.name]) {
          stats.backend[message.name]++
        } else {
          stats.backend[message.name] = 1
        }
      }
      sock.send(backend.buffer, bytes, 0)
      return
    }
    if (!backend.blocked) {
      just.error(backend.error.stack)
      backend.close()
    }
  }
  backend.onClose = () => {
    sock.close()
  }
}

async function main () {
  const sock = createSocket()
  if (sock.listen('0.0.0.0', 5432) !== 0) throw new SystemError('listen')
  let client = await sock.accept()
  while (client) {
    await startClient(client)
    client = await sock.accept()
  }
}

main().catch(err => just.error(err.stack))
