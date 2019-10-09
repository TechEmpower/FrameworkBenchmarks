import { Module } from '@nestjs/common';
import { TypeOrmModule } from '@nestjs/typeorm';

import { BenchController } from './bench.controller';
import { BenchService } from './bench.service';
import { WorldEntity } from './../models/world.entity';
import { FortuneEntity } from './../models/fortune.entity';

@Module({
  imports: [TypeOrmModule.forFeature([WorldEntity, FortuneEntity])],
  controllers: [BenchController],
  providers: [BenchService],
  exports: [BenchService],
})
export class BenchModule {}
