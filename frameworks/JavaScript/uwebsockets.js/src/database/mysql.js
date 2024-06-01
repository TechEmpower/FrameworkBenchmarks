import { createPool } from "mariadb";
import os from "node:os";

const pool = createPool({
  host: "tfb-database",
  user: "benchmarkdbuser",
  password: "benchmarkdbpass",
  database: "hello_world",
  connectionLimit: os.availableParallelism()
});

export const fortunes = async () => await pool.execute("SELECT id, message FROM fortune");

export const find = async (id) => await pool.execute("SELECT id, randomnumber FROM world WHERE id = ?", [id]).then((arr) => arr[0]);

export const bulkUpdate = async (worlds) => await Promise.all(worlds.map(world => pool.execute("UPDATE world SET randomnumber = ? WHERE id = ?", [world.randomNumber, world.id])));
