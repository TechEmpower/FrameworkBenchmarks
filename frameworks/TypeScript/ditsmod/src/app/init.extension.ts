import { Class, Extension, Logger, PerAppService, injectable } from '@ditsmod/core';

import { DbService } from './db.service.js';
import { ModelService } from './types.js';

@injectable()
export class InitExtension implements Extension<void> {
  constructor(
    private perAppService: PerAppService,
    private logger: Logger,
  ) {}

  async stage1(): Promise<void> {
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
  }

  protected async setDbService(useClass: Class) {
    const dbService = this.perAppService.injector
      .resolveAndCreateChild([DbService, { token: ModelService, useClass }])
      .get(DbService) as DbService;

    await dbService.setWorldsToCache();
    this.perAppService.providers.push({ token: DbService, useValue: dbService });
  }
}
