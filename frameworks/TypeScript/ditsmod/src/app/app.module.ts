import { PreRouter, Providers, rootModule } from '@ditsmod/core';

import { SimpleModule } from '#routed/simple/simple.module.js';
import { BunPreRouter } from './bun-integration/pre-router.js';

@rootModule({
  appends: [SimpleModule],
  providersPerApp: new Providers()
    .useLogConfig({ level: 'off' })
    .$if(global['Bun'])
    .useClass(PreRouter, BunPreRouter),
})
export class AppModule {}
