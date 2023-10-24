import { PoolConfig, createPool } from 'mariadb';
import { Fortune, ModelService, World } from './types.js';

const clientOpts: PoolConfig = {
  host: process.env.MYSQL_HOST,
  user: process.env.MYSQL_USER,
  password: process.env.MYSQL_PSWD,
  database: process.env.MYSQL_DBNAME,
};

const pool = createPool({ ...clientOpts, connectionLimit: 1 });
const execute = (text: string, values: string[]) => pool.execute(text, values || undefined);

export class MysqlService implements ModelService {
  fortunes() {
    return execute('select id, message from fortune', []) as Promise<Fortune[]>;
  }

  find(id: string) {
    return execute('select id, randomNumber from world where id = ?', [id]).then((arr) => arr[0]) as Promise<World>;
  }

  getAllWorlds() {
    return execute('select id, randomNumber from world', []) as Promise<World[]>;
  }

  bulkUpdate(worlds: World[]) {
    const sql = 'update world set randomNumber = ? where id = ?';
    const values = worlds.map((world) => [world.randomnumber, world.id]).sort((a, b) => (a[0] < b[0]) ? -1 : 1);
    return pool.batch(sql, values);
  }
}
