import { Class, Extension, Logger, PerAppService, injectable } from '@ditsmod/core';

import { DbService } from './db.service.js';
import { ModelService } from './types.js';

@injectable()
export class InitExtension implements Extension<void> {
  #inited: boolean;

  constructor(
    private perAppService: PerAppService,
    private logger: Logger,
  ) {}

  async init(): Promise<void> {
    if (this.#inited) {
      return;
    }

    const dbType = process.env.DATABASE as 'mysql' | 'postgres';

    if (dbType == 'mysql') {
      const { MysqlService } = await import('./mysql.service.js');
      await this.setDbService(MysqlService);
    } else if (dbType == 'postgres') {
      const { PostgresService } = await import('./postgres.service.js');
      await this.setDbService(PostgresService);
    } else {
      this.logger.log('warn', `Unknown database "${dbType}"`);
    }

    this.#inited = true;
  }

  protected async setDbService(useClass: Class) {
    const injector = this.perAppService.injector.resolveAndCreateChild([{ token: ModelService, useClass }]);
    const dbService = injector.pull(DbService) as DbService;
    await dbService.setWorldsToCache();
    this.perAppService.providers.push({ token: DbService, useValue: dbService });
  }
}
