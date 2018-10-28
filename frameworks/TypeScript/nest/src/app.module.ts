import { Module } from '@nestjs/common';
import { TypeOrmModule } from '@nestjs/typeorm';

import { AppController } from './app.controller';
import { AppService } from './app.service';
import { BenchModule } from './bench/bench.module';


@Module({
  imports: [TypeOrmModule.forRoot(), BenchModule],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}
