import { Server } from 'hyper-express'
import { LRUCache } from 'lru-cache'
import cluster, { isWorker } from 'node:cluster'
import { maxQuery, maxRows } from './config.js'
const { DATABASE } = process.env
const db = DATABASE ? await import(`./database/${DATABASE}.js`) : null

const generateRandomNumber = () => Math.floor(Math.random() * maxRows) + 1

const parseQueries = (i) => Math.min(parseInt(i) || 1, maxQuery)

const escapeHTMLRules = { '&': '&#38;', '<': '&#60;', '>': '&#62;', '"': '&#34;', "'": '&#39;', '/': '&#47;' }

const unsafeHTMLMatcher = /[&<>"'\/]/g

const escapeHTMLCode = (text) => unsafeHTMLMatcher.test(text) ? text.replace(unsafeHTMLMatcher, function (m) { return escapeHTMLRules[m] || m; }) : text

const cache = new LRUCache({
  max: maxRows
})

const app = new Server()

// use middleware to add `Server` into response header
app.use((_request, response, next) => {
  response.header('Server', 'hyperexpress')
  return next()
})

app.get('/plaintext', (_request, response) => {
  response.type('text').send('Hello, World!')
})

app.get('/json', (_request, response) => {
  response.json({ message: 'Hello, World!' })
})

if (db) {
  app.get('/db', async (_request, response) => {
    try {
      const world = await db.find(generateRandomNumber())

      response.json(world)
    } catch (error) {
      throw error
    }
  })

  app.get('/queries', async (request, response) => {
    try {
      const queries = parseQueries(request.query.queries)
      const worldPromises = new Array(queries)

      for (let i = 0; i < queries; i++) {
        worldPromises[i] = db.find(generateRandomNumber())
      }

      const worlds = await Promise.all(worldPromises);

      response.json(worlds)
    } catch (error) {
      throw error
    }
  })

  app.get('/updates', async (request, response) => {
    try {
      const queries = parseQueries(request.query.queries)
      const worldPromises = new Array(queries)

      for (let i = 0; i < queries; i++) {
        worldPromises[i] = db.find(generateRandomNumber())
      }

      const worlds = await Promise.all(worldPromises)

      for (let i = 0; i < queries; i++) {
        worlds[i].randomNumber = generateRandomNumber()
      }

      await db.bulkUpdate(worlds)

      response.json(worlds)
    } catch (error) {
      throw error
    }
  })

  app.get('/fortunes', async (_request, response) => {
    try {
      const fortunes = await db.fortunes()

      fortunes.push({ id: 0, message: 'Additional fortune added at request time.' })

      fortunes.sort((a, b) => (a.message < b.message) ? -1 : 1)

      const n = fortunes.length

      let i = 0, html = ''
      for (; i < n; i++) html += `<tr><td>${fortunes[i].id}</td><td>${escapeHTMLCode(fortunes[i].message)}</td></tr>`

      response
        .header('Content-Type', 'text/html; charset=utf-8')
        .send(`<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>${html}</table></body></html>`)
    } catch (error) {
      throw error
    }
  })

  let isCachePopulated = false
  app.get('/cached-worlds', async (request, response) => {
    try {
      if (!isCachePopulated) {
        const worlds = await db.getAllWorlds()
        for (let i = 0; i < worlds.length; i++) {
          cache.set(worlds[i].id, worlds[i])
        }
        isCachePopulated = true
      }
      const count = parseQueries(request.query.count)
      const worlds = new Array(count)

      for (let i = 0; i < count; i++) {
        worlds[i] = cache.get(generateRandomNumber())
      }

      response.json(worlds)
    } catch (error) {
      throw error
    }
  })
}

app.listen(8080).then(() => {
  console.log(`${isWorker ? `${cluster.worker.id}: ` : ''}Successfully bound to http://0.0.0.0:8080`)
})