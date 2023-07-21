import postgres from "postgres";

const sql = postgres({
  host: "tfb-database",
  user: "benchmarkdbuser",
  password: "benchmarkdbpass",
  database: "hello_world",
  max: 1
});

export const fortunes = () => sql`SELECT id, message FROM fortune`;

export const find = (id) => sql`SELECT id, randomNumber FROM world WHERE id = ${id}`.then((arr) => arr[0]);

export const update = (obj) => sql`UPDATE world SET randomNumber = ${obj.randomNumber} WHERE id = ${obj.id}`;
