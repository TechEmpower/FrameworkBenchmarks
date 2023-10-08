import { featureModule } from '@ditsmod/core';
import { RoutingModule } from '@ditsmod/routing';

import { DbModule } from '#service/db/db.module.js';
import { WithoutDbController } from './without-db.controller.js';
import { DbController } from './db.controller.js';
import { FortuneController } from './fortune.controller.js';
import { SingletonController } from './singleton.controller.js';
import { DbController2 } from './db.controller2.js';
import { FortuneController2 } from './fortune.controller2.js';

@featureModule({
  imports: [RoutingModule, DbModule],
  controllers: [
    WithoutDbController,
    DbController,
    DbController2,
    FortuneController,
    FortuneController2,
    SingletonController,
  ],
})
export class SimpleModule {}
