import postgres from 'postgres'
import { clientOpts } from '../config.js'

const sql = postgres({ ...clientOpts, max: 1 })

export const fortunes = async () => sql`SELECT id, message FROM fortune`

export const find = async (id) => sql`SELECT id, randomNumber FROM world WHERE id = ${id}`.then((arr) => arr[0])

export const getAllWorlds = async () => sql`SELECT id, randomNumber FROM world`

export const bulkUpdate = async (worlds) => await sql`UPDATE world SET randomNumber = (update_data.randomNumber)::int
  FROM (VALUES ${sql(worlds.map(world => [world.id, world.randomNumber]).sort((a, b) => (a[0] < b[0]) ? -1 : 1))}) AS update_data (id, randomNumber)
  WHERE world.id = (update_data.id)::int`;