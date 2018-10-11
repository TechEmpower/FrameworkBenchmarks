// tslint:disable:max-line-length
import { Config, ConnectionConfig, PoolConfig } from "knex";

const client: string = "pg"; // can also be "pg"
const pool: Readonly<PoolConfig> = { min: 2, max: 10 };
// const connection: Readonly<ConnectionConfig> = {
//   host: "tfb-database",
//   user: "benchmarkdbuser",
//   password: "benchmarkdbpass",
//   database: "hello_world",
// };
const connection: string = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world";

export default <Config> {
  client,
  connection,
  pool,
};
