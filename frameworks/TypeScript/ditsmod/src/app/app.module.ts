import { Providers, rootModule } from '@ditsmod/core';
import { SimpleModule } from '#routed/simple/simple.module.js';

@rootModule({
  appends: [SimpleModule],
  providersPerApp: [...new Providers().useLogConfig({ level: 'off' })],
})
export class AppModule {}
