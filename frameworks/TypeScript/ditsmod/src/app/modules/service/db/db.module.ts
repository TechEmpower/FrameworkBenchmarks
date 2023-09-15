import { PRE_ROUTER_EXTENSIONS, featureModule } from '@ditsmod/core';

import { DbService } from './db.service.js';
import { CacheService } from './cache.service.js';
import { InitExtension } from './init.extension.js';
import { DB_INIT_EXTENSIONS } from './tokens.js';

@featureModule({
  providersPerApp: [DbService, CacheService],
  extensions: [{ extension: InitExtension, groupToken: DB_INIT_EXTENSIONS, nextToken: PRE_ROUTER_EXTENSIONS }],
})
export class DbModule {}
