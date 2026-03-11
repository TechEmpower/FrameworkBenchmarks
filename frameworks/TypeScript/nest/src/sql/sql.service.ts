import { Injectable } from '@nestjs/common';
import { InjectRepository } from '@nestjs/typeorm';
import { Repository } from 'typeorm';
import { Fortune } from './fortune.entity';
import { World } from './world.entity';

@Injectable()
export class SqlService {
  constructor(
    @InjectRepository(World)
    private readonly worldRepository: Repository<World>,
    @InjectRepository(Fortune)
    private readonly fortuneRepository: Repository<Fortune>,
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
    const worlds = await Promise.all(
      Array.from({ length: number }, () => this.singleQuery()),
    );

    await Promise.all(
      worlds.map((world) => {
        world.randomnumber = Math.floor(Math.random() * 10000) + 1;
        return this.worldRepository.update(world.id, {
          randomnumber: world.randomnumber,
        });
      }),
    );

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
