import { Logger } from '@nestjs/common';
import { NestFactory } from '@nestjs/core';
import { NestExpressApplication } from '@nestjs/platform-express';
import {
  FastifyAdapter,
  NestFastifyApplication,
} from '@nestjs/platform-fastify';
import 'dotenv/config';
import { join } from 'path';
import { AppModule } from './app.module';
import cluster = require('cluster');
import os = require('os');

const port = process.env.PORT || 8080;

async function bootstrapExpress() {
  const app = await NestFactory.create<NestExpressApplication>(AppModule, {
    logger: false,
  });

  app.setBaseViewsDir(join(__dirname, '..', 'views'));
  app.setViewEngine('hbs');

  Logger.log(`Listening on port ${port}`, 'Nest Express Server');
  return app.listen(port);
}

async function bootstrapFastify() {
  const app = await NestFactory.create<NestFastifyApplication>(
    AppModule,
    new FastifyAdapter(),
    { logger: false },
  );
  app.setViewEngine({
    engine: {
      handlebars: require('handlebars'),
    },
    templates: join(__dirname, '..', 'views'),
  });
  await app.listen(8080, '0.0.0.0');
}

if (cluster.isMaster) {
  const cpus = os.cpus().length;
  for (let i = 0; i < cpus; i++) {
    cluster.fork();
  }

  Logger.log('NestJS master starting ' + new Date().toISOString());
  cluster.on('exit', () => {
    process.exit(1);
  });
} else {
  switch (process.env.FRAMEWORK) {
    case 'fastify':
      bootstrapFastify();
      Logger.log(`Worker fastify ${process.pid} started`);
      break;

    default:
      bootstrapExpress();
      Logger.log(`Worker express ${process.pid} started`);
      break;
  }
}
