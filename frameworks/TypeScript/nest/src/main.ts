import 'dotenv/config';
import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';
import { Logger } from '@nestjs/common';
import * as cluster from 'express-cluster';


const port = process.env.PORT || 8080;

async function bootstrap() {
  await cluster(async (w)=>{
    const app = await NestFactory.create(AppModule);
    Logger.log(`Listening on port ${port}`, 'Nest Server');
    return  app.listen(port)
  }, {});
}

bootstrap();