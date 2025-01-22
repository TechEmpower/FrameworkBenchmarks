import { InjectionToken, Providers, rootModule } from '@ditsmod/core';
import { PRE_ROUTER_EXTENSIONS, RoutingModule } from '@ditsmod/routing';

import { OneController } from './one.controller.js';
import { InitExtension } from './init.extension.js';

@rootModule({
  imports: [RoutingModule],
  providersPerApp: new Providers().useLogConfig({ level: 'off' }),
  extensions: [{ extension: InitExtension, group: new InjectionToken('test'), beforeGroups: [PRE_ROUTER_EXTENSIONS] }],
  controllers: [OneController],
})
export class AppModule {}
