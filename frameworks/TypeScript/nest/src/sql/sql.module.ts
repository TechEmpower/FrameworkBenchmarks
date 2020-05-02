import { CacheModule, Module } from '@nestjs/common';
import { TypeOrmModule } from '@nestjs/typeorm';
import { Fortune } from './fortune.entity';
import { SqlController } from './sql.controller';
import { SqlService } from './sql.service';
import { World } from './world.entity';

@Module({
  imports: [
    TypeOrmModule.forRoot(),
    TypeOrmModule.forFeature([World, Fortune]),
    CacheModule.register(),
  ],
  controllers: [SqlController],
  providers: [SqlService],
})
export class SqlModule {}
