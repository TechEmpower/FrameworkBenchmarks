import { Serve, Server } from 'bun';

import { AppModule } from './app/app.module.js';
import { BunApplication } from './app/bun-integration/bun-application.js';

const reusePort = process.env.NODE_ENV == 'production';
const serverOptions = { port: 8080, hostname: '0.0.0.0', reusePort } as Serve;
const { server } = await new BunApplication().bootstrap(AppModule, {
  serverOptions: serverOptions as any,
});

const bunServer = server as unknown as Server;
