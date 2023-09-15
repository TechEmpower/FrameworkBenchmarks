import { featureModule } from '@ditsmod/core';
import { RouterModule } from '@ditsmod/router';

import { DbModule } from '#service/db/db.module.js';
import { WithoutDbController } from './without-db.controller.js';
import { DbController } from './db.controller.js';
import { FortuneController } from './fortune.controller.js';

@featureModule({
  imports: [RouterModule, DbModule],
  controllers: [WithoutDbController, DbController, FortuneController]
})
export class SimpleModule {}
