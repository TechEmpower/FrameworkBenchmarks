import postgres from 'postgres'
import { clientOpts } from '../config.js'

const sql = postgres({ ...clientOpts, max: 1 })

export const fortunes = async () => sql`SELECT id, message FROM fortune`

export const find = async (id) => sql`SELECT id, randomNumber FROM world WHERE id = ${id}`.then((arr) => arr[0])

export const getAllWorlds = async () => sql`SELECT id, randomNumber FROM world`

export const update = async (obj) => sql`UPDATE world SET randomNumber = ${obj.randomNumber} WHERE id = ${obj.id}`
