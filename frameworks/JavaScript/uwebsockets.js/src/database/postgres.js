import Pool from "pg-pool";

const pool = new Pool({
  host: "tfb-database",
  user: "benchmarkdbuser",
  password: "benchmarkdbpass",
  database: "hello_world",
});

await pool.connect();

export const findOne = async (random) =>
  pool.query("SELECT * FROM world WHERE id = $1 LIMIT 1", [random]);
