import {
  CacheInterceptor,
  Controller,
  Get,
  Header,
  Query,
  Render,
  UseInterceptors,
} from '@nestjs/common';
import { AppService } from './app.service';

@Controller()
export class AppController {
  constructor(private readonly appService: AppService) {}

  @Get('/json')
  @Header('Server', 'NestJS')
  @Header('Content-Type', 'application/json')
  getJson() {
    return { message: 'Hello, World!' };
  }

  @Get('db')
  @Header('Server', 'NestJS')
  getSingleQuery() {
    return this.appService.singleQuery();
  }

  @Get('queries')
  @Header('Server', 'NestJS')
  getMultiQueries(@Query('queries') queries) {
    return this.appService.multiQueries(queries);
  }

  @Get('fortunes')
  @Header('Server', 'NestJS')
  @Render('fortunes.hbs')
  getFortunes() {
    return this.appService.fortunes();
  }

  @Get('updates')
  @Header('Server', 'NestJS')
  getUpdates(@Query('queries') queries) {
    return this.appService.updates(queries);
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
    return this.appService.cachedWorlds(count);
  }
}
