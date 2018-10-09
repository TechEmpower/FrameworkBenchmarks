import { Controller, Get, Header, Query } from '@nestjs/common';

import { BenchService } from './bench.service';

@Controller('bench')
export class BenchController {
    constructor(private benchService: BenchService){}

    @Get('/db')
    @Header('Server', 'NestJS')
    getRandomWorld(){

        return this.benchService.getOne();
    }

    @Get('/queries')
    @Header('Server', 'NestJS')
    getMultipleRandomWorlds(@Query('queries') query){

        return this.benchService.getMultiple(query);
    }

    @Get('/updates')
    @Header('Server', 'NestJS')
    updateRandomWorlds(@Query('queries') query){

        return this.benchService.updateMultiple(query);
    }

    @Get('/json')
    @Header('Server', 'NestJS')
    @Header('Content-Type', 'application/json')
    sayHello() {

        return JSON.stringify({message: 'Hello, World!'});
    }

    @Get('/plaintext')
    @Header('Server', 'NestJS')
    @Header('Content-Type', 'text/plain')
    plaintext() {

        return 'Hello, World!';
    }
}
