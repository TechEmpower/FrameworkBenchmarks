import { Logger } from '@nestjs/common';
import { NestFactory } from '@nestjs/core';
import { NestExpressApplication } from '@nestjs/platform-express';
import {
  FastifyAdapter,
  NestFastifyApplication,
} from '@nestjs/platform-fastify';
import { MongoModule } from './mongo/mongo.module';
import { join } from 'path';
import { SqlModule } from './sql/sql.module';
import cluster from 'cluster'
import os = require('os');

const port = process.env.PORT || 8080;

async function bootstrapExpress() {
  let app;
  if (process.env.DATABASE_CONFIGURATION_PROFILE === 'mongodb') {
    app = await NestFactory.create<NestExpressApplication>(MongoModule, {
      logger: false,
    });
  } else {
    app = await NestFactory.create<NestExpressApplication>(SqlModule, {
      logger: false,
    });
  }

  app.setBaseViewsDir(join(__dirname, '..', 'views'));
  app.setViewEngine('hbs');

  Logger.log(`Listening on port ${port}`, 'Nest Express Server');
  return app.listen(port);
}

async function bootstrapFastify() {
  let app;
  if (process.env.DATABASE_CONFIGURATION_PROFILE === 'mongodb') {
    app = await NestFactory.create<NestFastifyApplication>(
      MongoModule,
      new FastifyAdapter(),
      { logger: false },
    );
  } else {
    app = await NestFactory.create<NestFastifyApplication>(
      SqlModule,
      new FastifyAdapter(),
      { logger: false },
    );
  }

  app.setViewEngine({
    engine: {
      handlebars: require('handlebars'),
    },
    templates: join(__dirname, '..', 'views'),
  });
  await app.listen(8080, '0.0.0.0');
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
      bootstrapFastify();
      Logger.log(`Worker fastify ${process.pid} started`);
      break;

    default:
      bootstrapExpress();
      Logger.log(`Worker express ${process.pid} started`);
      break;
  }
}
