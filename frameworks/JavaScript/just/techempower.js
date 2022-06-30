const stringify = require('@stringify')
const html = require('@html')
const cache = require('@cache')
const dns = require('@dns')
const postgres = require('@pg')
const http = require('@http')
const socket = require('@socket')

const util = require('util.js')
const config = require('tfb.config.js')

const { getIPAddress } = dns
const { createSocket } = socket
const { createServer, responses } = http
const { SimpleCache } = cache
const { sprayer, sortByMessage, spawn, getUpdateQuery, Clock } = util
const { sjs, attr } = stringify
const { 
  db, fortunes, worlds, templates,
  maxQuery, maxRows, message, json,
  extra 
} = config

async function main () {
  const spray = sprayer(maxQuery)
  const getRandom = () => Math.ceil(Math.random() * maxRows)
  const getCount = (qs = { q: 1 }) => {
    return Math.min(parseInt((qs.q) || 1, 10), maxQuery) || 1
  }
  const sJSON = sjs({ message: attr('string') })
  const wJSON = sjs({ id: attr('number'), randomnumber: attr('number') })  
  const clock = new Clock()

  const sock = createSocket()
  const ip = await getIPAddress(db.hostname)
  await sock.connect(ip, db.port)
  const pg = await postgres.createSocket(sock, db)

  sock.noDelay = false

  const getWorldById = await pg.compile(worlds)
  const getFortunes = await pg.compile(fortunes)
  const worldCache = new SimpleCache(id => getWorldById(id))
  const template = html.load(templates.fortunes, templates.settings)
  const getRandomWorld = () => getWorldById(getRandom())
  const getCachedWorld = () => worldCache.get(getRandom())

  const server = createServer()
    .get('/plaintext', res => res.text(message))
    .get('/json', res => res.utf8(sJSON(json), responses.json))
    .get('/db', async res => {
      res.utf8(wJSON(await getRandomWorld()), responses.json)
    })
    .get('/fortunes', async res => {
      res.html(template.call(sortByMessage([extra, ...await getFortunes()])))
    })
    .get('/cached-world', async (res, req) => {
      res.json(await Promise.all(spray(getCount(req.query), getCachedWorld)))
    })
    .get('/query', async (res, req) => {
      res.json(await Promise.all(spray(getCount(req.query), getRandomWorld)))
    })
    .get('/update', async (res, req) => {
      const count = getCount(req.query)
      const worlds = await Promise.all(spray(count, getRandomWorld))
      const updateWorlds = await getUpdateQuery(count, pg)
      await updateWorlds(...worlds.map(w => {
        w.randomnumber = getRandom()
        return [w.id, w.randomnumber]
      }).flat())
      res.json(worlds)
    })
    .listen('0.0.0.0', 8080)

  clock.set(() => {
    worldCache.tick()
    server.update()
  })
}

spawn(main).catch(err => just.error(err.stack))
