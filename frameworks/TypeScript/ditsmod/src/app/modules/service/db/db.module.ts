import { featureModule } from '@ditsmod/core';
import { PRE_ROUTER_EXTENSIONS } from '@ditsmod/routing';

import { DbService } from './db.service.js';
import { InitExtension } from './init.extension.js';
import { DB_INIT_EXTENSIONS } from './tokens.js';
import { ModelService } from './types.js';

@featureModule({
  providersPerApp: [DbService, ModelService],
  extensions: [{ extension: InitExtension, groupToken: DB_INIT_EXTENSIONS, nextToken: PRE_ROUTER_EXTENSIONS }],
})
export class DbModule {}
