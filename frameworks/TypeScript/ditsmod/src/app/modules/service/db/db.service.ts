import knex, { Knex } from 'knex';
import { Logger, injectable } from '@ditsmod/core';

import { World } from './types.js';
import { getNumberOfObjects, getRandomNumber } from '#utils/helper.js';
import { CacheService } from './cache.service.js';

@injectable()
export class DbService {
  db: Knex;

  constructor(
    private cacheService: CacheService,
    private logger: Logger,
  ) {
    this.initDb();
  }

  protected async initDb() {
    const dbType = process.env.DATABASE as 'mysql' | 'postgres';
    if (dbType == 'mysql') {
      this.db = knex.default({
        client: 'mysql2',
        connection: {
          host: process.env.MYSQL_HOST,
          user: process.env.MYSQL_USER,
          password: process.env.MYSQL_PSWD,
          database: process.env.MYSQL_DBNAME,
        },
      });
    } else if (dbType == 'postgres') {
      this.db = knex.default({
        client: 'pg',
        connection: {
          host: process.env.PG_HOST,
          user: process.env.PG_USER,
          password: process.env.PG_PSWD,
          database: process.env.PG_DBNAME,
        },
      });
    } else {
      this.logger.log('warn', `Unknown database "${dbType}"`);
    }
  }

  findAllFortunes() {
    return this.db('Fortune').select('*');
  }

  /**
   * This method is called via `InitExtension` before the route handlers are created.
   */
  async setWorldsToCache(): Promise<void> {
    const { cache } = this.cacheService;
    const result = await this.db('World').select<World[]>('*');
    result.forEach((obj) => cache.set(obj.id, obj));
  }

  async findOneWorld(id: number, noCache = true): Promise<World> {
    if (noCache) {
      return this.db('World').first().where({ id });
    } else {
      let obj = this.cacheService.cache.get(id);
      if (obj) {
        return obj;
      }
      obj = await this.db('World').first<World>().where({ id });
      this.cacheService.cache.set(id, obj);
      return obj;
    }
  }

  getMultiQueries(queries: string, noCache = true) {
    const num = getNumberOfObjects(queries);
    const promisesArray = [];

    for (let i = 0; i < num; i++) {
      const id = getRandomNumber();
      promisesArray.push(this.findOneWorld(id, noCache));
    }

    return Promise.all(promisesArray);
  }

  async saveWorlds(queries: string) {
    const num = getNumberOfObjects(queries);
    const worldPromises = [];

    for (let i = 0; i < num; i++) {
      const id = getRandomNumber();
      worldPromises.push(this.findOneWorld(id));
    }

    const worlds = await Promise.all(worldPromises);

    const worldsToUpdate = worlds.map((world) => {
      world.randomnumber = getRandomNumber();
      return world;
    });

    const updates: Knex.QueryBuilder<any, number>[] = [];

    worldsToUpdate.forEach((world) => {
      const { id, randomnumber } = world;
      updates.push(this.db('World').update({ randomnumber }).where({ id }));
    });

    await Promise.all(updates);

    return worldsToUpdate;
  }
}
