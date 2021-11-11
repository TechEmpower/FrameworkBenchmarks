const { connect, constants } = require('lib/connection.js')
const { createServer } = require('lib/tcp.js')
const { createParser } = require('lib/http.js')
const { sjs, attr } = require('lib/stringify.js')

function compile (sock, query) {
  return new Promise((resolve, reject) => {
    const result = sock.compile(query, err => {
      if (err) return reject(err)
      resolve(result)
    })
  })
}

async function onPGAuth (sock) {
  sock.getWorldById = await compile(sock, {
    formats: [{ format: 1, oid: INT4OID }],
    sql: 'select id, randomNumber from World where id = $1',
    fields: [{ format: 1, oid: INT4OID }],
    name: 's1',
    portal: '',
    maxRows: 0,
    params: [1]
  })
  sock.allFortunes = await compile(sock, {
    formats: [],
    sql: 'select * from Fortune',
    fields: [{ format: 1, oid: INT4OID }, { format: 0, oid: VARCHAROID }],
    name: 's2',
    portal: '',
    maxRows: 0,
    htmlEscape: true,
    params: []
  })
  sock.updateWorldById = await compile(sock, {
    formats: [{ format: 1, oid: INT4OID }],
    sql: 'update World set randomNumber = $2 where id = $1',
    fields: [],
    name: 's3',
    portal: '',
    maxRows: 0,
    params: [1, 1]
  })
  // TODO: we could actually build these on the fly for any number of updates
  sock.updateWorldById20 = await compile(sock, {
    formats: [{ format: 1, oid: INT4OID }],
    sql: `update world set randomnumber = CASE id 
when $1 then $2 
when $3 then $4 
when $5 then $6 
when $7 then $8 
when $9 then $10 
when $11 then $12 
when $13 then $14 
when $15 then $16 
when $17 then $18 
when $19 then $20 
when $21 then $22 
when $23 then $24 
when $25 then $26 
when $27 then $28 
when $29 then $30 
when $31 then $32 
when $33 then $34 
when $35 then $36 
when $37 then $38 
when $39 then $40
else randomnumber 
end where id in ($1,$3,$5,$7,$9,$11,$13,$15,$17,$19,$21,$23,$25,$27,$29,$31,$33,$35,$37,$39)
`,
    fields: [],
    name: 's4',
    portal: '',
    maxRows: 0,
    params: Array(40).fill(0)
  })
  sock.updateWorldById15 = await compile(sock, {
    formats: [{ format: 1, oid: INT4OID }],
    sql: `update world set randomnumber = CASE id 
when $1 then $2 
when $3 then $4 
when $5 then $6 
when $7 then $8 
when $9 then $10 
when $11 then $12 
when $13 then $14 
when $15 then $16 
when $17 then $18 
when $19 then $20 
when $21 then $22 
when $23 then $24 
when $25 then $26 
when $27 then $28 
when $29 then $30 
else randomnumber 
end where id in ($1,$3,$5,$7,$9,$11,$13,$15,$17,$19,$21,$23,$25,$27,$29)
`,
    fields: [],
    name: 's5',
    portal: '',
    maxRows: 0,
    params: Array(30).fill(0)
  })
  sock.updateWorldById10 = await compile(sock, {
    formats: [{ format: 1, oid: INT4OID }],
    sql: `update world set randomnumber = CASE id 
when $1 then $2 
when $3 then $4 
when $5 then $6 
when $7 then $8 
when $9 then $10 
when $11 then $12 
when $13 then $14 
when $15 then $16 
when $17 then $18 
when $19 then $20 
else randomnumber 
end where id in ($1,$3,$5,$7,$9,$11,$13,$15,$17,$19)
`,
    fields: [],
    name: 's6',
    portal: '',
    maxRows: 0,
    params: Array(20).fill(0)
  })
  sock.updateWorldById5 = await compile(sock, {
    formats: [{ format: 1, oid: INT4OID }],
    sql: `update world set randomnumber = CASE id 
when $1 then $2 
when $3 then $4 
when $5 then $6 
when $7 then $8 
when $9 then $10 
else randomnumber 
end where id in ($1,$3,$5,$7,$9)
`,
    fields: [],
    name: 's7',
    portal: '',
    maxRows: 0,
    params: Array(10).fill(0)
  })
  sock.getCachedWorldById = await compile(sock, {
    formats: [{ format: 1, oid: INT4OID }],
    sql: 'select id, randomNumber from World where id = $1',
    fields: [{ format: 1, oid: INT4OID }],
    name: 's8',
    portal: '',
    maxRows: 0,
    params: [1]
  })
  clients.push(sock)
  if (clients.length === poolSize) onPGReady()
}

function onPGConnect (err, sock) {
  if (err) {
    just.error(err.stack)
    just.setTimeout(() => connect(tfb, onPGConnect), 1000)
    return
  }
  sock.onClose = () => {
    // todo: remove from pool and reconnect?
    just.error('pg.close')
  }
  sock.start(err => {
    if (err) return just.error(err.stack)
    sock.authenticate(err => {
      if (err) return just.error(err.stack)
      onPGAuth(sock).catch(err => just.error(err.stack))
    })
  })
}

const HEADER = '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'
const FOOTER = '</table></body></html>'
const S1 = '<tr><td>'
const S2 = '</td><td>'
const S3 = '</td></tr>'
function getHTML (rows) {
  let html = HEADER
  for (const row of rows) {
    html += (S1 + row[0] + S2 + row[1] + S3)
  }
  return html + FOOTER
}

function insertionSort (arr) {
  const n = arr.length
  for (let i = 1; i < n; i++) {
    const c = arr[i]
    let j = i - 1
    while ((j > -1) && (c[1] < arr[j][1])) {
      arr[j + 1] = arr[j]
      j--
    }
    arr[j + 1] = c
  }
  return arr
}

const cache = {}

function onHTTPConnect (sock) {
  const client = clients[sock.fd % clients.length]
  const rbuf = new ArrayBuffer(4096)
  const parser = createParser(rbuf)
  const { getWorldById, updateWorldById, allFortunes, updateWorldById20, updateWorldById15, updateWorldById10, updateWorldById5, getCachedWorldById } = client
  const message = { message: 'Hello, World!' }
  const text = 'Hello, World!'
  const extra = [0, 'Additional fortune added at request time.']
  const updateQueries = {
    5: updateWorldById5,
    10: updateWorldById10,
    15: updateWorldById15,
    20: updateWorldById20
  }
  const results = []
  let queries = 0
  let updates = 0
  function onUpdateMulti () {
    const json = JSON.stringify(results)
    sock.writeString(`${rJSON}${json.length}${END}${json}`)
  }
  function onUpdateSingle () {
    updates++
    if (results.length === updates) {
      const json = JSON.stringify(results)
      sock.writeString(`${rJSON}${json.length}${END}${json}`)
    }
  }
  function onUpdates () {
    const [id, randomNumber] = getWorldById.getRows()[0]
    results.push({ id, randomNumber })
    if (results.length === queries) {
      const query = updateQueries[queries]
      if (query) {
        let i = 0
        for (const row of results) {
          row.randomNumber = Math.ceil(Math.random() * 10000)
          query.params[i++] = row.id
          query.params[i++] = row.randomNumber
        }
        query.call(onUpdateMulti)
        return
      }
      updates = 0
      for (const row of results) {
        row.randomNumber = Math.ceil(Math.random() * 10000)
        updateWorldById.params[0] = row.id
        updateWorldById.params[1] = row.randomNumber
        updateWorldById.append(onUpdateSingle)
      }
      updateWorldById.send()
    }
  }
  function handleUpdates (qs) {
    const [, val] = qs.split('=')
    queries = Math.min(parseInt(val || 1, 10), 500) || 1
    results.length = 0
    for (let i = 1; i < queries; i++) {
      getWorldById.params[0] = Math.ceil(Math.random() * 10000)
      getWorldById.append(onUpdates, (i % 20 === 0))
    }
    getWorldById.params[0] = Math.ceil(Math.random() * 10000)
    getWorldById.append(onUpdates)
    getWorldById.send()
  }
  function onMulti () {
    const [id, randomNumber] = getWorldById.getRows()[0]
    results.push({ id, randomNumber })
    if (results.length === queries) {
      const json = JSON.stringify(results)
      sock.writeString(`${rJSON}${json.length}${END}${json}`)
      queries = 0
    }
  }
  function handleMulti (qs) {
    const [, val] = qs.split('=')
    queries = Math.min(parseInt(val || 1, 10), 500) || 1
    results.length = 0
    for (let i = 1; i < queries; i++) {
      getWorldById.params[0] = Math.ceil(Math.random() * 10000)
      getWorldById.append(onMulti, (i % 20 === 0))
    }
    getWorldById.params[0] = Math.ceil(Math.random() * 10000)
    getWorldById.append(onMulti)
    getWorldById.send()
  }
  function onCached () {
    const row = getCachedWorldById.getRows()[0]
    const [id, randomNumber] = row
    const world = { id, randomNumber }
    cache[id] = world
    results.push(world)
    if (results.length === queries) {
      const json = JSON.stringify(results)
      sock.writeString(`${rJSON}${json.length}${END}${json}`)
      queries = 0
      results.length = 0
    }
  }
  function handleCached (qs) {
    const [, val] = qs.split('=')
    queries = Math.min(parseInt(val || 1, 10), 500) || 1
    for (let i = 1; i < queries; i++) {
      const id = Math.ceil(Math.random() * 10000)
      const row = cache[id]
      if (row) {
        results.push(row)
      } else {
        getCachedWorldById.params[0] = id
        getCachedWorldById.append(onCached, (i % 20 === 0))
      }
    }
    const id = Math.ceil(Math.random() * 10000)
    const row = cache[id]
    if (row) {
      results.push(row)
    } else {
      getCachedWorldById.params[0] = id
      getCachedWorldById.append(onCached)
    }
    if (results.length === queries) {
      const json = JSON.stringify(results)
      sock.writeString(`${rJSON}${json.length}${END}${json}`)
      queries = 0
      results.length = 0
      return
    }
    getCachedWorldById.send()
  }
  function onFortunes () {
    const html = getHTML(insertionSort([extra, ...allFortunes.getRows()]))
    sock.writeString(`${rHTML}${utf8Length(html)}${END}${html}`)
  }
  function onSingle () {
    const [id, randomNumber] = getWorldById.getRows()[0]
    const json = sDB({ id, randomNumber })
    sock.writeString(`${rJSON}${json.length}${END}${json}`)
  }
  const queryPath = '/query'
  const updatePath = '/update'
  const cachePath = '/cached-world'
  const pathSep = '?'
  const END = '\r\n\r\n'
  const handlers = {
    '/json': () => {
      const json = sJSON(message)
      sock.writeString(`${rJSON}${json.length}${END}${json}`)
    },
    '/fortunes': () => allFortunes.call(onFortunes),
    '/db': () => {
      getWorldById.params[0] = Math.ceil(Math.random() * 10000)
      getWorldById.call(onSingle)
    },
    '/plaintext': () => sock.writeString(`${rTEXT}${text.length}${END}${text}`),
    default: url => {
      const [path, qs] = url.split(pathSep)
      if (path === queryPath) {
        handleMulti(qs)
        return
      }
      if (path === updatePath) {
        handleUpdates(qs)
        return
      }
      if (path === cachePath) {
        handleCached(qs)
        return
      }
      sock.writeString(r404)
    }
  }
  parser.onRequests = count => {
    if (count > 1) {
      sock.writeString(`${rTEXT}${text.length}${END}${text}`.repeat(count))
      return
    }
    const url = parser.url(0)
    const handler = (handlers[url] || handlers.default)
    handler(url)
  }
  sock.onData = bytes => parser.parse(bytes)
  sock.onClose = () => {
    parser.free()
  }
  return parser.buffer
}

function onPGReady () {
  microtasks = false
  just.print(`listen: ${server.listen()}`)
}

const { utf8Length } = just.sys
const poolSize = parseInt(just.env().PGPOOL || just.sys.cpus, 10)
const server = createServer('0.0.0.0', 8080)
server.onConnect = onHTTPConnect
const { INT4OID, VARCHAROID } = constants.fieldTypes
const clients = []
const tfb = {
  hostname: 'tfb-database',
  port: 5432,
  user: 'benchmarkdbuser',
  pass: 'benchmarkdbpass',
  database: 'hello_world'
}
let i = poolSize
const sJSON = sjs({ message: attr('string') })
const sDB = sjs({ id: attr('number'), randomNumber: attr('number') })
while (i--) connect(tfb, onPGConnect)
const { loop } = just.factory
let microtasks = true

let time = (new Date()).toUTCString()
let rHTML = `HTTP/1.1 200 OK\r\nServer: j\r\nDate: ${time}\r\nContent-Type: text/html; charset=UTF-8\r\nContent-Length: `
let rTEXT = `HTTP/1.1 200 OK\r\nServer: j\r\nDate: ${time}\r\nContent-Type: text/plain\r\nContent-Length: `
let rJSON = `HTTP/1.1 200 OK\r\nServer: j\r\nDate: ${time}\r\nContent-Type: application/json\r\nContent-Length: `
let r404 = `HTTP/1.1 404 Not Found\r\nServer: j\r\nDate: ${time}\r\nContent-Type: text/plain\r\nContent-Length: 0\r\n\r\n`
just.setInterval(() => {
  time = (new Date()).toUTCString()
  rHTML = `HTTP/1.1 200 OK\r\nServer: j\r\nDate: ${time}\r\nContent-Type: text/html; charset=UTF-8\r\nContent-Length: `
  rTEXT = `HTTP/1.1 200 OK\r\nServer: j\r\nDate: ${time}\r\nContent-Type: text/plain\r\nContent-Length: `
  rJSON = `HTTP/1.1 200 OK\r\nServer: j\r\nDate: ${time}\r\nContent-Type: application/json\r\nContent-Length: `
  r404 = `HTTP/1.1 404 Not Found\r\nServer: j\r\nDate: ${time}\r\nContent-Type: text/plain\r\nContent-Length: 0\r\n\r\n`
}, 100)
while (1) {
  if (loop.poll(0) === 0) loop.poll(-1)
  if (microtasks) just.sys.runMicroTasks()
}
