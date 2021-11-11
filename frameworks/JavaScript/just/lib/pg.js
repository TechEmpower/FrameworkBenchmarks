const md5 = require('md5.js')

function syncMessage () {
  const len = 5
  const buf = new ArrayBuffer(len)
  const dv = new DataView(buf)
  dv.setUint8(0, 83)
  dv.setUint32(1, 4)
  return buf
}

function startupMessage ({ user, database, parameters = [] }) {
  let len = 8 + 4 + 1 + user.length + 1 + 8 + 1 + database.length + 2
  for (let i = 0; i < parameters.length; i++) {
    const { name, value } = parameters[i]
    len += (name.length + 1 + value.length + 1)
  }
  const buf = new ArrayBuffer(len)
  const dv = new DataView(buf)
  let off = 0
  dv.setInt32(0, 0)
  off += 4
  // 0x00030000 = 3.0
  dv.setInt32(4, 196608)
  off += 4

  off += buf.writeString('user', off)
  dv.setUint8(off++, 0)
  off += buf.writeString(user, off)
  dv.setUint8(off++, 0)

  off += buf.writeString('database', off)
  dv.setUint8(off++, 0)
  off += buf.writeString(database, off)
  dv.setUint8(off++, 0)

  for (let i = 0; i < parameters.length; i++) {
    const { name, value } = parameters[i]
    off += buf.writeString(name, off)
    dv.setUint8(off++, 0)
    off += buf.writeString(value, off)
    dv.setUint8(off++, 0)
  }
  dv.setUint8(off++, 0)
  dv.setInt32(0, off)
  return buf
}

function md5AuthMessage ({ user, pass, salt }) {
  const token = `${pass}${user}`
  let hash = md5(token)
  const plain = new ArrayBuffer(36)
  plain.writeString(`md5${hash}`, 0)
  const plain2 = new ArrayBuffer(36)
  plain2.copyFrom(plain, 0, 32, 3)
  plain2.copyFrom(salt, 32, 4)
  hash = `md5${md5(plain2)}`
  const len = hash.length + 5
  let off = 0
  const buf = new ArrayBuffer(len + 1)
  const dv = new DataView(buf)
  dv.setUint8(off++, 112)
  dv.setUint32(off, len)
  off += 4
  off += buf.writeString(hash, off)
  dv.setUint8(off++, 0)
  return buf
}

function createParser (buf) {
  let nextRow = 0
  let parseNext = 0
  let parameters = {}
  const query = { start: 0, end: 0, rows: 0, running: false }

  if (freeList.length) return freeList.shift()

  function onDataRow (len, off) {
    // D = DataRow
    nextRow++
    return off + len - 4
  }

  function onCommandComplete (len, off) {
    // C = CommandComplete
    query.end = off
    query.rows = nextRow
    query.running = false
    off += len - 4
    nextRow = 0
    parser.onMessage()
    return off
  }

  function onRowDescripton (len, off) {
    // T = RowDescription
    const fieldCount = dv.getInt16(off)
    off += 2
    fields.length = 0
    for (let i = 0; i < fieldCount; i++) {
      const name = readCString(buf, u8, off)
      off += name.length + 1
      const tid = dv.getInt32(off)
      off += 4
      const attrib = dv.getInt16(off)
      off += 2
      const oid = dv.getInt32(off)
      off += 4
      const size = dv.getInt16(off)
      off += 2
      const mod = dv.getInt32(off)
      off += 4
      const format = dv.getInt16(off)
      off += 2
      fields.push({ name, tid, attrib, oid, size, mod, format })
    }
    parser.onMessage()
    return off
  }

  function onAuthenticationOk (len, off) {
    // R = AuthenticationOk
    const method = dv.getInt32(off)
    off += 4
    if (method === constants.AuthenticationMD5Password) {
      parser.salt = buf.slice(off, off + 4)
      off += 4
      parser.onMessage()
    }
    return off
  }

  function onErrorResponse (len, off) {
    // E = ErrorResponse
    errors.length = 0
    let fieldType = u8[off++]
    while (fieldType !== 0) {
      const val = readCString(buf, u8, off)
      errors.push({ type: fieldType, val })
      off += (val.length + 1)
      fieldType = u8[off++]
    }
    parser.onMessage()
    return off
  }

  function onParameterStatus (len, off) {
    // S = ParameterStatus
    const key = readCString(buf, u8, off)
    off += (key.length + 1)
    const val = readCString(buf, u8, off)
    off += val.length + 1
    parameters[key] = val
    return off
  }

  function onParameterDescription (len, off) {
    // t = ParameterDescription
    const nparams = dv.getInt16(off)
    parser.params = []
    off += 2
    for (let i = 0; i < nparams; i++) {
      parser.params.push(dv.getUint32(off))
      off += 4
    }
    return off
  }

  function onParseComplete (len, off) {
    // 1 = ParseComplete
    off += len - 4
    parser.onMessage()
    return off
  }

  function onBindComplete (len, off) {
    // 2 = BindComplete
    off += len - 4
    parser.onMessage()
    query.rows = 0
    query.start = query.end = off
    query.running = true
    return off
  }

  function onReadyForQuery (len, off) {
    // Z = ReadyForQuery
    parser.status = u8[off]
    parser.onMessage()
    off += len - 4
    return off
  }

  function onBackendKeyData (len, off) {
    // K = BackendKeyData
    parser.pid = dv.getUint32(off)
    off += 4
    parser.key = dv.getUint32(off)
    off += 4
    parser.onMessage()
    return off
  }

  function parse (bytesRead) {
    let type
    let len
    let off = parseNext
    const end = buf.offset + bytesRead
    while (off < end) {
      const remaining = end - off
      let want = 5
      if (remaining < want) {
        if (byteLength - off < 1024) {
          if (query.running) {
            const queryLen = off - query.start + remaining
            buf.copyFrom(buf, 0, queryLen, query.start)
            buf.offset = queryLen
            parseNext = off - query.start
            query.start = 0
            return
          }
          buf.copyFrom(buf, 0, remaining, off)
          buf.offset = remaining
          parseNext = 0
          return
        }
        buf.offset = off + remaining
        parseNext = off
        return
      }
      type = parser.type = dv.getUint8(off)
      len = parser.len = dv.getUint32(off + 1)
      want = len + 1
      if (remaining < want) {
        if (byteLength - off < 1024) {
          if (query.running) {
            const queryLen = off - query.start + remaining
            buf.copyFrom(buf, 0, queryLen, query.start)
            buf.offset = queryLen
            parseNext = off - query.start
            query.start = 0
            return
          }
          buf.copyFrom(buf, 0, remaining, off)
          buf.offset = remaining
          parseNext = 0
          return
        }
        buf.offset = off + remaining
        parseNext = off
        return
      }
      off += 5
      off = (V[type] || V[0])(len, off)
    }
    parseNext = buf.offset = 0
  }

  function getResult () {
    return readCString(buf, u8, parseNext)
  }

  function onDefault (len, off) {
    off += len - 4
    parser.onMessage()
    return off
  }

  function free () {
    parser.fields.length = 0
    parser.errors.length = 0
    parameters = parser.parameters = {}
    nextRow = 0
    parseNext = 0
    query.start = query.end = query.rows = 0
    query.running = false
    freeList.push(parser)
  }

  const { messageTypes } = constants
  const dv = new DataView(buf)
  const u8 = new Uint8Array(buf)
  const byteLength = buf.byteLength
  const fields = []
  const errors = []
  const V = {
    [messageTypes.AuthenticationOk]: onAuthenticationOk,
    [messageTypes.ErrorResponse]: onErrorResponse,
    [messageTypes.RowDescription]: onRowDescripton,
    [messageTypes.CommandComplete]: onCommandComplete,
    [messageTypes.ParseComplete]: onParseComplete,
    [messageTypes.BindComplete]: onBindComplete,
    [messageTypes.ReadyForQuery]: onReadyForQuery,
    [messageTypes.BackendKeyData]: onBackendKeyData,
    [messageTypes.ParameterStatus]: onParameterStatus,
    [messageTypes.ParameterDescription]: onParameterDescription,
    [messageTypes.DataRow]: onDataRow,
    0: onDefault
  }
  const parser = {
    buf,
    dv,
    fields,
    parameters,
    type: 0,
    len: 0,
    errors,
    getResult,
    parse,
    free,
    query
  }
  return parser
}

function readCString (buf, u8, off) {
  const start = off
  while (u8[off] !== 0) off++
  return buf.readString(off - start, start)
}

function getPGError (errors) {
  return errors.filter(v => v.type === 77)[0].val
}

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
    NoData: 110
  }
}

const freeList = []

module.exports = { createParser, syncMessage, startupMessage, md5AuthMessage, getPGError, constants }
