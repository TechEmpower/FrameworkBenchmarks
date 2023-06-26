import Pool from "pg-pool";

const pool = new Pool({
  host: "tfb-database",
  user: "benchmarkdbuser",
  password: "benchmarkdbpass",
  database: "hello_world",
});

await pool.connect();

const query = (text, values) =>
  pool.query(text, values || []).then((r) => r.rows);

export const fortunes = () => query("SELECT * FROM fortune");

export const find = (id) =>
  query("SELECT * FROM world WHERE id = $1", [id]).then((arr) => arr[0]);

export const update = (obj) =>
  query("UPDATE world SET randomNumber = $1 WHERE id = $2", [
    obj.randomNumber,
    obj.id,
  ]);
