import { Logger } from '@nestjs/common';
import { NestFactory } from '@nestjs/core';
import { NestExpressApplication } from '@nestjs/platform-express';
import {
  FastifyAdapter,
  NestFastifyApplication,
} from '@nestjs/platform-fastify';
import { join } from 'path';
import cluster from 'node:cluster';
import os from 'node:os';
import handlebars from 'handlebars';
import fastifyView from '@fastify/view';
import { MongoModule } from './mongo/mongo.module';
import { SqlModule } from './sql/sql.module';

const port = Number(process.env.PORT ?? 8080);

async function bootstrapExpress() {
  const appModule =
    process.env.DATABASE_CONFIGURATION_PROFILE === 'mongodb'
      ? MongoModule
      : SqlModule;
  const app = await NestFactory.create<NestExpressApplication>(appModule, {
    logger: false,
  });
  app.getHttpServer().keepAliveTimeout = 0;

  app.setBaseViewsDir(join(__dirname, '..', 'views'));
  app.setViewEngine('hbs');

  Logger.log(`Listening on port ${port}`, 'Nest Express Server');
  await app.listen(port);
}

async function bootstrapFastify() {
  const appModule =
    process.env.DATABASE_CONFIGURATION_PROFILE === 'mongodb'
      ? MongoModule
      : SqlModule;
  const app = await NestFactory.create<NestFastifyApplication>(
    appModule,
    new FastifyAdapter({ logger: false }),
    { logger: false },
  );
  app.getHttpServer().keepAliveTimeout = 0;

  await app.register(fastifyView as never, {
    engine: {
      handlebars,
    },
    root: join(__dirname, '..', 'views'),
  });

  await app.listen({ port, host: '0.0.0.0' });
  Logger.log(`Listening on port ${port}`, 'Nest Fastify Server');
}

if (cluster.isPrimary) {
  const cpus = os.cpus().length;
  for (let i = 0; i < cpus; i++) {
    cluster.fork();
  }

  Logger.log(`NestJS master ${process.pid} started`);
  cluster.on('exit', () => {
    process.exit(1);
  });
} else {
  switch (process.env.FRAMEWORK) {
    case 'fastify':
      void bootstrapFastify();
      Logger.log(`Worker fastify ${process.pid} started`);
      break;

    default:
      void bootstrapExpress();
      Logger.log(`Worker express ${process.pid} started`);
      break;
  }
}
