import cluster from 'node:cluster';
import type { ServerOptions } from 'node:http';
import { availableParallelism } from 'node:os';
import { Application } from '@ditsmod/core';

import { AppModule } from './app/app.module.js';

const numCpus = availableParallelism();

if (numCpus > 1 && cluster.isPrimary) {
  for (let i = 0; i < numCpus; i++) {
    cluster.fork();
  }
} else {
  const serverOptions: ServerOptions = { keepAlive: true, keepAliveTimeout: 0 };
  const app = await new Application().bootstrap(AppModule, { serverOptions });
  app.server.listen(8080, '0.0.0.0');
}
