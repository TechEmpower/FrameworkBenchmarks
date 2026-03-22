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
    return this.worldRepository.findOneBy({ id: rand });
  }

  async multiQueries(queries?: string) {
    const number = this.parseQueryCount(queries);
    return Promise.all(Array.from({ length: number }, () => this.singleQuery()));
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

  async updates(queries?: string) {
    const number = this.parseQueryCount(queries);
    const bulk = this.worldRepository.initializeUnorderedBulkOp();
    const worlds = await Promise.all(
      Array.from({ length: number }, () => this.singleQuery()),
    );

    for (const worldToUpdate of worlds) {
      worldToUpdate.randomNumber = Math.floor(Math.random() * 10000) + 1;
      bulk
        .find({ id: worldToUpdate.id })
        .update({ $set: { randomNumber: worldToUpdate.randomNumber } });
    }

    await bulk.execute();
    return worlds;
  }

  async cachedWorlds(count?: string) {
    const number = this.parseQueryCount(count);
    return Promise.all(Array.from({ length: number }, () => this.singleQuery()));
  }

  private parseQueryCount(value?: string) {
    return Math.min(Math.max(Number.parseInt(value ?? '', 10) || 1, 1), 500);
  }
}
