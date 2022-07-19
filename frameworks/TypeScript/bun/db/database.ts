import { HelloWorldDatabase } from './types.ts';
import { Pool } from 'pg';
import { Kysely, PostgresDialect } from 'kysely';

const dialect = new PostgresDialect({
  pool: new Pool({
    connectionString: 'postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world',
    max: 50,
  }),
});

export const db = new Kysely<HelloWorldDatabase>({
  dialect,
});
