const process = require('process')
const postgres = require('@pg')

const { constants } = postgres
const { BinaryInt } = constants

/**
 * Generate a Bulk Update SQL statement definition For a given table, identity 
 * column and column to be updated, it will generate a single SQL
 * statement to update all fields in one statement
 *
 * @param {string} table   - The name of the table
 * @param {string} field   - The name of the field we want to update
 * @param {string} id      - The name of the id field
 * @param {string} updates - The number of rows to update in the statement
 * @param {string} type    - The name of the table
 */
function generateBulkUpdate (table, field, id, updates = 5, formats = [BinaryInt]) {
  function getIds (count) {
    const updates = []
    for (let i = 1; i < (count * 2); i += 2) {
      updates.push(`$${i}`)
    }
    return updates.join(',')
  }
  function getClauses (count) {
    const clauses = []
    for (let i = 1; i < (count * 2); i += 2) {
      clauses.push(`when $${i} then $${i + 1}`)
    }
    return clauses.join('\n')
  }
  const sql = []
  sql.push(`update ${table} set ${field} = CASE ${id}`)
  sql.push(getClauses(updates))
  sql.push(`else ${field}`)
  sql.push(`end where ${id} in (${getIds(updates)})`)
  return {
    formats,
    fields: [],
    name: `bulk.${updates}`,
    portal: '',
    params: Array(updates * 2).fill(0),
    sql: sql.join('\n'),
    sync: true
  }
}

/**
 * Utility function to generate an array of N values populated with provided
 * map function. There seems to be no simpler/quicker way to do this in JS.
 * @param {string} n     - Size of the array to create
 * @param {string} field - The map function which will create each array value
 */
function sprayer (max = 100) {
  const ar = [0]
  for (let i = 0; i < max; i++) {
    ar[i + 1] = (new Array(i + 1)).fill(1)
  }
  max += 1
  return (n, fn) => ar[n % max].map(fn)
}

function sortByMessage (arr) {
  const n = arr.length
  for (let i = 1; i < n; i++) {
    const c = arr[i]
    let j = i - 1
    while ((j > -1) && (c.message < arr[j].message)) {
      arr[j + 1] = arr[j]
      j--
    }
    arr[j + 1] = c
  }
  return arr
}

function spawn (main) {
  if (just.env()['WORKER']) return main()
  const { watch, launch } = process
  const processes = []
  const cpus = parseInt(just.env().CPUS || just.sys.cpus, 10)
  for (let i = 0; i < cpus; i++) {
    just.sys.setenv('WORKER', i)
    //const proc = launch(just.args[0], ['--trace-gc', ...just.args.slice(1)])
    const proc = launch(just.args[0], just.args.slice(1))
    processes.push(proc)
    proc.stats = { user: 0, system: 0 }
  }
  return Promise.all(processes.map(p => watch(p)))
}

const updates = new Map()

function getUpdateQuery (count, pg, formats = [BinaryInt]) {
  const query = updates.get(count)
  if (query) return query
  const promise = pg.compile(generateBulkUpdate('world', 'randomnumber', 'id', count, formats))
  updates.set(count, promise)
  return promise
}

class Clock {
  constructor () {
    this.slots = new Map()
  }

  unset (callback, repeat = 1000) {
    const current = this.slots.get(repeat)
    if (!current) return
    current.callbacks = current.callbacks.filter(cb => cb !== callback)
    if (!current.callbacks.length) {
      just.clearTimeout(current.timer)
      this.slots.delete(repeat)
    }
  }

  set (callback, repeat = 1000) {
    let current = this.slots.get(repeat)
    if (current) {
      current.callbacks.push(callback)
      return
    }
    current = {
      callbacks: [callback],
      timer: just.setInterval(() => current.callbacks.forEach(cb => cb()), repeat)
    }
    this.slots.set(repeat, current)
  }
}

module.exports = {
  sprayer,
  spawn,
  sortByMessage,
  generateBulkUpdate,
  getUpdateQuery,
  Clock
}
