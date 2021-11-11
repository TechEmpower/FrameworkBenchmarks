import {
  CacheInterceptor,
  Controller,
  Get,
  Header,
  Query,
  Render,
  UseInterceptors,
} from '@nestjs/common';
import { MongoService } from './mongo.service';

@Controller()
export class MongoController {
  constructor(private readonly mongoService: MongoService) {}

  @Get('/json')
  @Header('Server', 'NestJS')
  @Header('Content-Type', 'application/json')
  getJson() {
    return { message: 'Hello, World!' };
  }

  @Get('db')
  @Header('Server', 'NestJS')
  getSingleQuery() {
    return this.mongoService.singleQuery();
  }

  @Get('queries')
  @Header('Server', 'NestJS')
  getMultiQueries(@Query('queries') queries) {
    return this.mongoService.multiQueries(queries);
  }

  @Get('fortunes')
  @Header('Server', 'NestJS')
  @Render('fortunes.hbs')
  getFortunes() {
    return this.mongoService.fortunes();
  }

  @Get('updates')
  @Header('Server', 'NestJS')
  getUpdates(@Query('queries') queries) {
    return this.mongoService.updates(queries);
  }

  @Get('plaintext')
  @Header('Server', 'NestJS')
  @Header('Content-Type', 'text/plain')
  getHello(): string {
    return 'Hello, World!';
  }

  @Get('/cached-worlds')
  @Header('Server', 'NestJS')
  @UseInterceptors(CacheInterceptor)
  getCachedWorlds(@Query('count') count) {
    return this.mongoService.cachedWorlds(count);
  }
}
