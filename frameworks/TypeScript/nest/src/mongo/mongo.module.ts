import { CacheModule, Module } from '@nestjs/common';
import { TypeOrmModule } from '@nestjs/typeorm';
import { Fortune } from './fortune.entity';
import { MongoController } from './mongo.controller';
import { MongoService } from './mongo.service';
import { World } from './world.entity';

@Module({
  imports: [
    TypeOrmModule.forRoot(),
    TypeOrmModule.forFeature([World, Fortune]),
    CacheModule.register(),
  ],
  controllers: [MongoController],
  providers: [MongoService],
})
export class MongoModule {}
