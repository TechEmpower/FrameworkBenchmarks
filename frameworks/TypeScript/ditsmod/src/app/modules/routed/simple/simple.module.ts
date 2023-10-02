import { featureModule } from '@ditsmod/core';
import { RoutingModule } from '@ditsmod/routing';

import { DbModule } from '#service/db/db.module.js';
import { WithoutDbController } from './without-db.controller.js';
import { DbController } from './db.controller.js';
import { FortuneController } from './fortune.controller.js';
import { SingletonController } from './singleton.controller.js';

@featureModule({
  imports: [RoutingModule, DbModule],
  controllers: [WithoutDbController, DbController, FortuneController, SingletonController],
})
export class SimpleModule {}
