<<<<<<< HEAD
<<<<<<< HEAD
// tslint:disable:max-line-length
import { Config, ConnectionConfig, PoolConfig } from "knex";

const client: string = "pg"; // can also be "postgresql"
<<<<<<< HEAD
const pool: Readonly<PoolConfig> = { min: 2, max: 10 };
// const connection: Readonly<ConnectionConfig> = {
//   host: "tfb-database",
//   user: "benchmarkdbuser",
//   password: "benchmarkdbpass",
//   database: "hello_world",
// };
const connection: string = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world";
=======
=======
// tslint:disable:max-line-length
>>>>>>> Final push for the day
import { Config, ConnectionConfig, PoolConfig } from "knex";

const client: string = "pg"; // can also be "pg"
=======
>>>>>>> Fixes broken tests
const pool: Readonly<PoolConfig> = { min: 2, max: 10 };
<<<<<<< HEAD
const connection: Readonly<ConnectionConfig> = {
  host: "tfb-database",
  user: "benchmarkdbuser",
  password: "benchmarkdbpass",
  database: "hello_world",
};
>>>>>>> Wraps up initial server configuration
=======
// const connection: Readonly<ConnectionConfig> = {
//   host: "tfb-database",
//   user: "benchmarkdbuser",
//   password: "benchmarkdbpass",
//   database: "hello_world",
// };
const connection: string = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world";
>>>>>>> Final push for the day

export default <Config> {
  client,
  connection,
  pool,
};
