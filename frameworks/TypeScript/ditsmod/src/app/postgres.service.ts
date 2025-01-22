import postgres from 'postgres';
import { Fortune, ModelService, World } from './types.js';

const clientOpts: postgres.Options<any> = {
  host: process.env.PG_HOST,
  user: process.env.PG_USER,
  password: process.env.PG_PSWD,
  database: process.env.PG_DBNAME,
};

const sql = postgres({ ...clientOpts, max: 1 });

export class PostgresService implements ModelService {
  fortunes() {
    return sql`select id, message from fortune` as Promise<Fortune[]>;
  }

  find(id: string) {
    return sql`select id, randomNumber from world where id = ${id}`.then((arr) => arr[0]) as Promise<World>;
  }

  getAllWorlds() {
    return sql`select id, randomNumber from world` as Promise<World[]>;
  }

  bulkUpdate(worlds: World[]) {
    const values = sql(worlds.map((world) => [world.id, world.randomnumber]).sort((a, b) => (a[0] < b[0]) ? -1 : 1));

    return sql`update world set randomNumber = (update_data.randomNumber)::int
      from (values ${values}) as update_data (id, randomNumber)
      where world.id = (update_data.id)::int`;
  }
}
