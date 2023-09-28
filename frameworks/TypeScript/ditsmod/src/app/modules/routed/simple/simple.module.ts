import { Router, featureModule } from '@ditsmod/core';
import { RouterModule } from '@ditsmod/router';

import { DbModule } from '#service/db/db.module.js';
import { WithoutDbController } from './without-db.controller.js';
import { DbController } from './db.controller.js';
import { FortuneController } from './fortune.controller.js';

@featureModule({
  imports: [RouterModule, DbModule],
  controllers: [WithoutDbController, DbController, FortuneController],
})
export class SimpleModule {
  constructor(router: Router) {
    router
      .on('GET', '/plaintext2', async (nodeReq, nodeRes) => {
        nodeRes.setHeader('Server', 'Ditsmod');
        nodeRes.setHeader('Content-Type', 'text/plain; charset=utf-8');
        nodeRes.end('Hello, World!');
      })
      .on('GET', '/json2', async (nodeReq, nodeRes) => {
        nodeRes.setHeader('Server', 'Ditsmod');
        nodeRes.setHeader('Content-Type', 'application/json; charset=utf-8');
        nodeRes.end(JSON.stringify({ message: 'Hello, World!' }));
      });
  }
}
