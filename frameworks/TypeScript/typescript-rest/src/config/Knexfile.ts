import { Config, ConnectionConfig, PoolConfig } from "knex";

const client: string = "postgresql"; // can also be "pg"
const pool: Readonly<PoolConfig> = { min: 2, max: 10 };
const connection: Readonly<ConnectionConfig> = {
  host: "tfb-database",
  user: "benchmarkdbuser",
  password: "benchmarkdbpass",
  database: "hello_world",
};

export default <Config> {
  client,
  connection,
  pool,
};
