import { PreRouter, rootModule } from '@ditsmod/core';

import { SimpleModule } from '#routed/simple/simple.module.js';
import { BunPreRouter } from './bun-integration/pre-router.js';
import { BunProviders } from './bun-integration/bun-providers.js';

@rootModule({
  appends: [SimpleModule],
  providersPerApp: [
    ...new BunProviders().useLogConfig({ level: 'off' }).if(process.env.IS_BUN).useClass(PreRouter, BunPreRouter),
  ],
})
export class AppModule {}
