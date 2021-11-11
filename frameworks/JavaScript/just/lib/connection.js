const { lookup } = require('lookup.js')
const { createClient } = require('tcp.js')
const { md5AuthMessage, syncMessage, startupMessage, createParser, getPGError, constants } = require('pg.js')
const { html } = just.library('html.so', 'html')

const {
  AuthenticationOk,
  ErrorResponse,
  RowDescription,
  CommandComplete,
  ParseComplete,
  NoData,
  ReadyForQuery
} = constants.messageTypes

const { INT4OID } = constants.fieldTypes

function getMessageName (type) {
  const code = String.fromCharCode(type)
  let name = ''
  Object.keys(constants.messageTypes).some(key => {
    if (constants.messageTypes[key] === type) {
      name = key
      return true
    }
  })
  return { type, code, name }
}

function setupSocket (sock, config) {
  function compile (query, onComplete) {
    const buf = new ArrayBuffer(4096)
    const dv = new DataView(buf)
    let len = 0
    const fun = {
      dv,
      size: 0,
      described: false,
      buffer: new ArrayBuffer(65536),
      messages: {
        prepare: { start: 0, len: 0 },
        bind: { start: 0, len: 0 },
        exec: { start: 0, len: 0 },
        describe: { start: 0, len: 0 },
        flush: { start: 0, len: 0 },
        sync: { start: 0, len: 0 }
      },
      paramStart: 0
    }
    fun.buffer.offset = 0
    const { name, sql, params = [], formats = [], fields = [], portal = '', maxRows = 0 } = query
    fun.call = (onComplete, syncIt = true, flushIt = false) => {
      let off = fun.paramStart
      // 32 bit integers only for now
      for (let i = 0; i < params.length; i++) {
        off += 4
        dv.setUint32(off, params[i])
        off += 4
      }
      const { bind, exec, flush, sync } = fun.messages
      off = bind.start
      let len = 0
      if (flushIt) {
        len = flush.start + flush.len - off
      } else if (syncIt) {
        len = sync.start + sync.len - off
      } else {
        len = exec.start + exec.len - off
      }
      const r = sock.write(buf, len, off)
      if (r < len) {
        just.error('short write')
      }
      callbacks.push(onComplete)
    }
    fun.append = (onComplete, syncIt = true, flushIt = false) => {
      let off = fun.paramStart
      // 32 bit integers only for now
      for (let i = 0; i < params.length; i++) {
        off += 4
        dv.setUint32(off, params[i])
        off += 4
      }
      const { bind, exec, flush, sync } = fun.messages
      off = bind.start
      let len = 0
      if (flushIt) {
        len = flush.start + flush.len - off
      } else if (syncIt) {
        len = sync.start + sync.len - off
      } else {
        len = exec.start + exec.len - off
      }
      fun.buffer.offset += fun.buffer.copyFrom(buf, fun.buffer.offset, len, off)
      callbacks.push(onComplete)
    }
    fun.send = () => {
      const r = sock.write(fun.buffer, fun.buffer.offset, 0)
      if (r < len) {
        just.error('short write')
      }
      fun.buffer.offset = 0
    }
    fun.bind = (flushIt = true, onComplete) => {
      const { bind, flush } = fun.messages
      sock.write(buf, bind.len, bind.start)
      if (flushIt) {
        sock.write(buf, flush.len, flush.start)
      }
      callbacks.push(onComplete)
    }
    fun.exec = (flushIt = true, onComplete) => {
      const { exec, flush } = fun.messages
      sock.write(buf, exec.len, exec.start)
      if (flushIt) {
        sock.write(buf, flush.len, flush.start)
      }
      callbacks.push(onComplete)
    }
    fun.prepare = (flushIt = true, onComplete) => {
      const { prepare, flush } = fun.messages
      sock.write(buf, prepare.len, prepare.start)
      if (flushIt) {
        sock.write(buf, flush.len, flush.start)
      }
      callbacks.push(onComplete)
    }
    fun.describe = (flushIt = true, onComplete) => {
      const { describe, flush } = fun.messages
      sock.write(buf, describe.len, describe.start)
      if (flushIt) {
        sock.write(buf, flush.len, flush.start)
      }
      callbacks.push(onComplete)
    }
    let off = 0
    // Prepare Message
    fun.messages.prepare.start = off
    len = 1 + 4 + sql.length + 1 + name.length + 1 + 2 + (formats.length * 4)
    dv.setUint8(off++, 80) // 'P'
    dv.setUint32(off, len - 1)
    off += 4
    off += buf.writeString(name, off)
    dv.setUint8(off++, 0)
    off += buf.writeString(sql, off)
    dv.setUint8(off++, 0)
    dv.setUint16(off, formats.length)
    off += 2
    for (let i = 0; i < formats.length; i++) {
      dv.setUint32(off, formats[i].oid)
      off += 4
    }
    fun.messages.prepare.len = off - fun.messages.prepare.start
    // Describe Message
    fun.messages.describe.start = off
    len = 7 + name.length
    dv.setUint8(off++, 68) // 'D'
    dv.setUint32(off, len - 1)
    off += 4
    dv.setUint8(off++, 83) // 'S'
    off += buf.writeString(name, off)
    dv.setUint8(off++, 0)
    fun.messages.describe.len = off - fun.messages.describe.start

    // Bind Message
    fun.messages.bind.start = off
    dv.setUint8(off++, 66) // 'B'
    off += 4 // length - will be filled in later
    if (portal.length) {
      off += buf.writeString(portal, off)
      dv.setUint8(off++, 0)
      off += buf.writeString(name, off)
      dv.setUint8(off++, 0)
    } else {
      dv.setUint8(off++, 0)
      off += buf.writeString(name, off)
      dv.setUint8(off++, 0)
    }
    dv.setUint16(off, formats.length || 0)
    off += 2
    for (let i = 0; i < formats.length; i++) {
      dv.setUint16(off, formats[i].format)
      off += 2
    }
    dv.setUint16(off, params.length || 0)
    off += 2
    fun.paramStart = off
    for (let i = 0; i < params.length; i++) {
      if ((formats[i] || formats[0]).format === 1) {
        dv.setUint32(off, 4)
        off += 4
        dv.setUint32(off, params[i])
        off += 4
      } else {
        const paramString = params[i].toString()
        dv.setUint32(off, paramString.length)
        off += 4
        off += buf.writeString(paramString, off)
      }
    }
    dv.setUint16(off, fields.length)
    off += 2
    for (let i = 0; i < fields.length; i++) {
      dv.setUint16(off, fields[i].format)
      off += 2
    }
    fun.messages.bind.len = off - fun.messages.bind.start
    dv.setUint32(fun.messages.bind.start + 1, fun.messages.bind.len - 1)
    // Exec Message
    fun.messages.exec.start = off
    len = 6 + portal.length + 4
    dv.setUint8(off++, 69) // 'E'
    dv.setUint32(off, len - 1)
    off += 4
    if (portal.length) {
      off += buf.writeString(portal, off)
    }
    dv.setUint8(off++, 0)
    dv.setUint32(off, maxRows)
    off += 4
    fun.messages.exec.len = off - fun.messages.exec.start
    // Sync Message
    fun.messages.sync.start = off
    dv.setUint8(off++, 83) // 'S'
    dv.setUint32(off, 4)
    off += 4
    fun.messages.sync.len = off - fun.messages.sync.start
    // Flush Message
    fun.messages.flush.start = off
    dv.setUint8(off++, 72) // 'H'
    dv.setUint32(off, 4)
    off += 4
    fun.messages.flush.len = off - fun.messages.flush.start
    fun.size = off
    fun.buf = buf.slice(0, off)
    Object.assign(query, fun)
    let readString = just.sys.readString
    if (query.htmlEscape) {
      readString = html.escape
    }
    query.getRows = () => {
      const { buf, dv } = parser
      const { fields } = query
      const { start, rows } = parser.query
      let off = start
      const result = []
      let i = 0
      let j = 0
      let row
      for (i = 0; i < rows; i++) {
        off += 5
        const cols = dv.getUint16(off)
        off += 2
        row = Array(cols)
        result.push(row)
        for (j = 0; j < cols; j++) {
          len = dv.getUint32(off)
          const { oid, format } = (fields[j] || fields[0])
          off += 4
          if (format === 0) { // Non-Binary
            if (oid === INT4OID) {
              row[j] = parseInt(buf.readString(len, off), 10)
            } else {
              row[j] = readString(buf, len, off)
            }
          } else {
            if (oid === INT4OID) {
              row[j] = dv.getInt32(off)
            } else {
              row[j] = buf.slice(off, off + len)
            }
          }
          off += len
        }
      }
      return result
    }
    query.getResult = () => parser.getResult()
    if (!onComplete) return query
    fun.prepare(true, err => {
      if (err) return onComplete(err)
      fun.describe(true, err => {
        if (err) return onComplete(err)
        onComplete()
      })
    })
    return query
  }

  function start (onStart) {
    callbacks.push(onStart)
    sock.write(startupMessage(config))
  }

  function authenticate (onAuthenticate) {
    callbacks.push(onAuthenticate)
    sock.write(md5AuthMessage({ user, pass, salt: parser.salt }))
  }

  function onMessage () {
    const { type } = parser
    if (type === CommandComplete) {
      callbacks.shift()()
      return
    }
    if (type === ReadyForQuery) {
      if (!sock.authenticated) {
        sock.authenticated = true
        callbacks.shift()()
      }
      return
    }
    if (type === ErrorResponse) {
      callbacks.shift()(new Error(getPGError(parser.errors)))
      return
    }
    if (type === AuthenticationOk || type === ParseComplete || type === RowDescription || type === NoData) callbacks.shift()()
  }

  const buf = new ArrayBuffer(64 * 1024)
  sock.authenticated = false
  const parser = sock.parser = createParser(buf)
  const callbacks = []
  const { user, pass } = config
  parser.onMessage = onMessage
  sock.authenticate = authenticate
  sock.sync = () => sock.write(syncMessage())
  sock.start = start
  sock.compile = compile
  sock.onData = bytes => parser.parse(bytes)
  sock.onClose = () => {
    just.error('pg socket closed')
  }
  sock.getParams = () => parser.parameters
  sock.size = () => callbacks.length
  sock.query = parser.query
  sock.buffer = buf
  return sock
}

function connect (config, onPGConnect) {
  lookup(config.hostname, (err, ip) => {
    if (err) {
      onPGConnect(err)
      return
    }
    config.address = ip
    const sock = createClient(config.address, config.port)
    sock.onClose = () => {
      just.error('pg socket closed')
    }
    sock.onConnect = err => {
      onPGConnect(err, setupSocket(sock, config))
      return sock.buffer
    }
    sock.connect()
  })
}

module.exports = { connect, constants, getMessageName }
