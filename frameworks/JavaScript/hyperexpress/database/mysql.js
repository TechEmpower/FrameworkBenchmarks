import { createPool, createConnection } from 'mysql2/promise'
import { isWorker } from 'node:cluster'
import { cpus } from 'node:os'
import { clientOpts } from '../config.js'

const client = await createConnection(clientOpts)

const res = await client.query('SHOW VARIABLES LIKE "max_connections"')

let maxConnections = 150

if (isWorker) {
    maxConnections = cpus().length > 2 ? Math.ceil(res[0][0].Value * 0.96 / cpus().length) : maxConnections
}

await client.end()

const pool = createPool(Object.assign({ ...clientOpts }, {
    connectionLimit: maxConnections,
    idleTimeout: 600000
}))

const execute = async (text, values) => (await pool.execute(text, values || undefined))[0]

export const fortunes = async () => execute('SELECT * FROM fortune')

export const find = async (id) => execute('SELECT id, randomNumber FROM world WHERE id = ?', [id]).then(arr => arr[0])

export const getAllWorlds = async () => execute('SELECT * FROM world')

export const update = async (obj) => execute('UPDATE world SET randomNumber = ? WHERE id = ?', [obj.randomNumber, obj.id])

await Promise.all([...Array(maxConnections).keys()].map(fortunes))