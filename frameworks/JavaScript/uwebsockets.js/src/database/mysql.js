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

export const findOne = async (random) =>
  pool.execute("SELECT * FROM world WHERE id = ? LIMIT 1", [random]);
