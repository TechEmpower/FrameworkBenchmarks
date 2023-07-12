import { escape } from 'html-escaper'
import { Server } from 'hyper-express'
import { LRUCache } from 'lru-cache'
import cluster, { isWorker } from 'node:cluster'
import { maxQuery, maxRows } from './config.js'
const { DATABASE } = process.env
const db = DATABASE ? await import(`./database/${DATABASE}.js`) : null

const generateRandomNumber = () => Math.ceil(Math.random() * maxRows)

const parseQueries = (i) => Math.min(Math.max(parseInt(i, 10) || 1, 1), maxQuery)

const cache = new LRUCache({
  max: maxRows
})

const app = new Server()

// use middleware to add `Server` into response header
app.use((_request, response, next) => {
  response.header('Server', 'hyperexpress')
  next()
})

app.get('/plaintext', (_request, response) => {
  response.atomic(() => {
    response
      .type('text')
      .send('Hello, World!')
  })
})

app.get('/json', (_request, response) => {
  response.json({ message: 'Hello, World!' })
})

if (db) {
  // populate cache
  (async () => {
    const worlds = await db.getAllWorlds()
    for (let i = 0; i < worlds.length; i++) {
      cache.set(worlds[i].id, worlds[i])
    }
  })()

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
      const worldPromises = []

      for (let i = 0; i < queries; i++) {
        worldPromises.push(db.find(generateRandomNumber()))
      }

      const worlds = await Promise.all(worldPromises)
      response.json(worlds)
    } catch (error) {
      throw error
    }
  })

  app.get('/updates', async (request, response) => {
    try {
      const queries = parseQueries(request.query.queries)
      const worldPromises = []

      for (let i = 0; i < queries; i++) {
        worldPromises.push(db.find(generateRandomNumber()))
      }

      const worlds = await Promise.all(worldPromises)

      const updatedWorlds = await Promise.all(worlds.map(async (world) => {
        world.randomNumber = generateRandomNumber()
        await db.update(world)
        return world
      }))
      response.json(updatedWorlds)
    } catch (error) {
      throw error
    }
  })

  app.get('/fortunes', async (_request, response) => {
    try {
      const fortunes = await db.fortunes()

      fortunes.push({ id: 0, message: 'Additional fortune added at request time.' })

      fortunes.sort((a, b) => a.message.localeCompare(b.message))

      let i = 0, html = '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'
      for (; i < fortunes.length; i++) html += `<tr><td>${fortunes[i].id}</td><td>${escape(fortunes[i].message)}</td></tr>`
      html += '</table></body></html>'

      response.atomic(() => {
        response
          // .type('html')
          .header('Content-Type', 'text/html; charset=utf-8')
          .send(html)
      })
    } catch (error) {
      throw error
    }
  })

  app.get('/cached-worlds', async (request, response) => {
    try {
      const count = parseQueries(request.query.count)
      const worlds = []

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