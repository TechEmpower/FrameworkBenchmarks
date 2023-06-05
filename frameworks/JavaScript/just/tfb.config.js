const postgres = require('@pg')

const { constants } = postgres
const { BinaryInt, VarChar } = constants

const db = {
  hostname: 'tfb-database',
  user: 'benchmarkdbuser',
  pass: 'benchmarkdbpass',
  database: 'hello_world',
  version: constants.PG_VERSION,
  port: 5432
}

const fortunes = {
  portal: '',
  formats: [],
  name: 'fortunes',
  maxRows: 0,
  params: [],
  sql: 'select * from Fortune',
  fields: [
    { format: BinaryInt, name: 'id' },
    { format: VarChar, name: 'message', htmlEscape: true }
  ]
}

const worlds = {
  portal: '',
  formats: [BinaryInt],
  name: 'worlds',
  maxRows: 0,
  params: [0],
  sql: 'select id, randomNumber from World where id = $1',
  fields: [
    { format: BinaryInt, name: 'id' },
    { format: BinaryInt, name: 'randomnumber' }
  ]
}

const templates = {
  fortunes: 'fortunes.html',
  settings: { rawStrings: false, compile: true }
}

const maxQuery = 500
const maxRows = 10000
const message = 'Hello, World!'
const json = { message }
const extra = { id: 0, message: 'Additional fortune added at request time.' }

module.exports = {
  db, fortunes, worlds, templates,
  maxQuery, maxRows, message, json,
  extra 
}
