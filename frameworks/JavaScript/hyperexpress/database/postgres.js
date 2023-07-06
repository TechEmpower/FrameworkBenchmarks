import { isWorker } from 'node:cluster'
import { cpus } from 'node:os'
import postgres from 'postgres'
import { clientOpts } from '../config.js'

const sql = postgres(clientOpts)

const res = await sql`SHOW max_connections`

let maxConnections = 150

if (isWorker) {
  maxConnections = cpus().length > 2 ? Math.ceil(res[0].max_connections * 0.96 / cpus().length): maxConnections 
}

export const fortunes = async () => sql`SELECT * FROM fortune`

export const find = async (id) => sql`SELECT id, randomNumber FROM world WHERE id = ${id}`.then((arr) => arr[0])

export const getAllWorlds = async () => sql`SELECT * FROM world`

export const update = async (obj) => sql`UPDATE world SET randomNumber = ${obj.randomNumber} WHERE id = ${obj.id}`

await Promise.all([...Array(maxConnections).keys()].map(fortunes))