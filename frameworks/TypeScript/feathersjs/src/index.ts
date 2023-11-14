import cluster from 'cluster';
import os from 'os';

import logger from './logger';
import app from './app';

if (cluster.isPrimary) {
  const cpuCount: os.CpuInfo[] = os.cpus();

  for (const cpu of cpuCount) {
    cluster.fork();
  }

  cluster.on('exit', () => {
    process.exit(1);
  });
} else {

  const port = app.get('port');
  const server = app.listen(port);

  process.on('unhandledRejection', (reason, p) =>
    logger.error('Unhandled Rejection at: Promise ', p, reason)
  );

  server.on('listening', () =>
    logger.info('Feathers application started on http://%s:%d', app.get('host'), port)
  );
}

