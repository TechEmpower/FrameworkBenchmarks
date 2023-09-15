import { Extension, PerAppService, injectable } from '@ditsmod/core';
import { DbService } from './db.service.js';

/**
 * Used to prepare the cache before creating route handlers.
 */
@injectable()
export class InitExtension implements Extension<void> {
  #inited: boolean;

  constructor(
    private dbService: DbService,
    private perAppService: PerAppService,
  ) {}

  async init(): Promise<void> {
    if (this.#inited) {
      return;
    }

    if (process.env.DATABASE) {
      await this.dbService.setWorldsToCache();
      this.perAppService.providers.push({ token: DbService, useValue: this.dbService });
    }

    this.#inited = true;
  }
}
