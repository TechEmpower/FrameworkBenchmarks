import { createPool } from 'mariadb'
import { cpus } from 'node:os'
import { clientOpts } from '../config.js'

const pool = createPool({ ...clientOpts, connectionLimit: cpus().length * 2 + 1 })

const execute = (text, values) => pool.execute(text, values || undefined)

export const fortunes = () => execute('SELECT id, message FROM fortune')

export const find = (id) => execute('SELECT id, randomNumber FROM world WHERE id = ?', [id]).then(arr => arr[0])

export const getAllWorlds = () => execute('SELECT id, randomNumber FROM world')

export const update = (obj) => execute('UPDATE world SET randomNumber = ? WHERE id = ?', [obj.randomNumber, obj.id])
