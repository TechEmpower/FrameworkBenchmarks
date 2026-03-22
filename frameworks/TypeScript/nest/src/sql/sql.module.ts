import { CacheModule } from '@nestjs/cache-manager';
import { Module } from '@nestjs/common';
import { TypeOrmModule } from '@nestjs/typeorm';
import { createTypeOrmOptions } from '../database/typeorm-options';
import { Fortune } from './fortune.entity';
import { SqlController } from './sql.controller';
import { SqlService } from './sql.service';
import { World } from './world.entity';

@Module({
  imports: [
    TypeOrmModule.forRoot(createTypeOrmOptions()),
    TypeOrmModule.forFeature([World, Fortune]),
    CacheModule.register(),
  ],
  controllers: [SqlController],
  providers: [SqlService],
})
export class SqlModule {}
