import { injectable } from '@ditsmod/core';
import { LRUCache } from 'lru-cache';

import { ModelService, World } from './types.js';
import { getNumberOfObjects, getRandomNumber } from './helper.js';

@injectable()
export class DbService {
  #cache = new LRUCache<number, World>({ max: 10000 });

  constructor(private modelService: ModelService) {}

  findAllFortunes() {
    return this.modelService.fortunes();
  }

  /**
   * This method is called via `InitExtension` before the route handlers are created.
   */
  async setWorldsToCache(): Promise<void> {
    const result = await this.modelService.getAllWorlds();
    result.forEach((obj) => this.#cache.set(obj.id, obj));
  }

  async findOneWorld(id: number, noCache = true): Promise<World> {
    if (noCache) {
      return this.modelService.find(`${id}`);
    } else {
      let obj = this.#cache.get(id);
      if (obj) {
        return obj;
      }
      obj = await this.modelService.find(`${id}`);
      this.#cache.set(id, obj);
      return obj!;
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

    await this.modelService.bulkUpdate(worldsToUpdate);

    return worldsToUpdate;
  }
}
