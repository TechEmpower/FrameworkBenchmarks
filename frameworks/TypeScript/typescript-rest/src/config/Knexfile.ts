// tslint:disable:max-line-length
import { Config, PoolConfig } from "knex";

const client: string = "pg"; // can also be "postgresql"
const pool: Readonly<PoolConfig> = { min: 2, max: 10 };
const connection: string = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world";

export default <Config> {
  client,
  connection,
  pool,
};
