import { createPool } from "mysql2/promise";

const pool = createPool({
  host: "tfb-database",
  user: "benchmarkdbuser",
  password: "benchmarkdbpass",
  database: "hello_world",
  waitForConnections: true,
  connectionLimit: 512,
  queueLimit: 0,
});

const query = (text, values) =>
  pool.execute(text, values || []).then((r) => r[0]);

export const fortunes = () => query("SELECT * FROM fortune");

export const find = (id) =>
  query("SELECT * FROM world WHERE id = ?", [id]).then((arr) => arr[0]);

export const update = (obj) =>
  query("UPDATE world SET randomNumber = ? WHERE id = ?", [
    obj.randomNumber,
    obj.id,
  ]);
