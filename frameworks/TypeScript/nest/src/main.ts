import 'dotenv/config';
import { NestFactory } from '@nestjs/core';
import { NestExpressApplication } from '@nestjs/platform-express';
import { AppModule } from './app.module';
import { join } from 'path';
import { Logger } from '@nestjs/common';
import * as cluster from 'express-cluster';


const port = process.env.PORT || 8080;

async function bootstrap() {
  await cluster(async (w)=>{
    const app = await NestFactory.create<NestExpressApplication>(AppModule);

    app.setBaseViewsDir(join(__dirname, 'views'));
    app.setViewEngine('hbs');

    Logger.log(`Listening on port ${port}`, 'Nest Server');
    return  app.listen(port)
  }, {});
}

bootstrap();