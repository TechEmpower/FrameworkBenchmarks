import { Injectable } from '@nestjs/common';
import { InjectRepository } from '@nestjs/typeorm';
import { MongoRepository } from 'typeorm';
import { Fortune } from './fortune.entity';
import { World } from './world.entity';

@Injectable()
export class MongoService {
  constructor(
    @InjectRepository(World)
    private readonly worldRepository: MongoRepository<World>,
    @InjectRepository(Fortune)
    private readonly fortuneRepository: MongoRepository<Fortune>,
  ) {}

  singleQuery() {
    const rand = Math.floor(Math.random() * 10000) + 1;
    return this.worldRepository.findOne({ id: rand });
  }

  async multiQueries(queries: string) {
    const number = Math.min(Math.max(parseInt(queries) || 1, 1), 500);
    const promisesArray = [];

    for (let i = 0; i < number; i++) {
      promisesArray.push(this.singleQuery());
    }

    const worlds = await Promise.all(promisesArray);
    return worlds;
  }

  async fortunes() {
    const allFortunes = await this.fortuneRepository.find();
    allFortunes.push({
      id: 0,
      message: 'Additional fortune added at request time.',
    });

    allFortunes.sort((a, b) => (a.message < b.message ? -1 : 1));
    return { fortunes: allFortunes };
  }

  async updates(queries) {
    const number = Math.min(Math.max(parseInt(queries) || 1, 1), 500);
    const worlds = [];
    const bulk = this.worldRepository.initializeUnorderedBulkOp();

    for (let i = 0; i < number; i++) {
      const worldToUpdate = await this.singleQuery();
      worldToUpdate.randomNumber = Math.floor(Math.random() * 10000) + 1;
      worlds.push(worldToUpdate);
      bulk
        .find({ id: worldToUpdate.id })
        .update({ $set: { randomNumber: worldToUpdate.randomNumber } });
    }

    await bulk.execute();
    return worlds;
  }

  async cachedWorlds(count) {
    const number = Math.min(Math.max(parseInt(count) || 1, 1), 500);
    const promisesArray = [];
    for (let i = 0; i < number; i++) {
      promisesArray.push(this.singleQuery());
    }

    const worlds = await Promise.all(promisesArray);
    return worlds;
  }
}
