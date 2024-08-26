import { Providers, rootModule } from '@ditsmod/core';
import { PRE_ROUTER_EXTENSIONS, RoutingModule } from '@ditsmod/routing';

import { OneController } from './one.controller.js';
import { DbService } from './db.service.js';
import { InitExtension } from './init.extension.js';
import { DB_INIT_EXTENSIONS } from './tokens.js';
import { ModelService } from './types.js';

@rootModule({
  imports: [RoutingModule],
  providersPerApp: new Providers().passThrough(DbService).passThrough(ModelService).useLogConfig({ level: 'off' }),
  extensions: [{ extension: InitExtension, groupToken: DB_INIT_EXTENSIONS, nextToken: PRE_ROUTER_EXTENSIONS }],
  controllers: [OneController],
})
export class AppModule {}
