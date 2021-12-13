import {
  CacheInterceptor,
  Controller,
  Get,
  Header,
  Query,
  Render,
  UseInterceptors,
} from '@nestjs/common';
import { SqlService } from './sql.service';

@Controller()
export class SqlController {
  constructor(private readonly sqlService: SqlService) {}

  @Get('/json')
  @Header('Server', 'NestJS')
  @Header('Content-Type', 'application/json')
  getJson() {
    return { message: 'Hello, World!' };
  }

  @Get('db')
  @Header('Server', 'NestJS')
  getSingleQuery() {
    return this.sqlService.singleQuery();
  }

  @Get('queries')
  @Header('Server', 'NestJS')
  getMultiQueries(@Query('queries') queries) {
    return this.sqlService.multiQueries(queries);
  }

  @Get('fortunes')
  @Header('Server', 'NestJS')
  @Render('fortunes.hbs')
  getFortunes() {
    return this.sqlService.fortunes();
  }

  @Get('updates')
  @Header('Server', 'NestJS')
  getUpdates(@Query('queries') queries) {
    return this.sqlService.updates(queries);
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
    return this.sqlService.cachedWorlds(count);
  }
}
