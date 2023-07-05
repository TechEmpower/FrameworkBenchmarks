import postgres from 'postgres'

const clientOpts = {
  host: 'tfb-database',
  user: 'benchmarkdbuser',
  password: 'benchmarkdbpass',
  database: 'hello_world',
}

const sql = postgres(clientOpts);

export const fortunes = async () => await sql`SELECT * FROM fortune`

export const find = async (id) =>
  await sql`SELECT id, randomNumber FROM world WHERE id = ${id}`.then((arr) => arr[0])

export const getAllWorlds = async () =>
  await sql`SELECT * FROM world`

export const update = async (obj) =>
  await sql`UPDATE world SET randomNumber = ${obj.randomNumber} WHERE id = ${obj.id}`