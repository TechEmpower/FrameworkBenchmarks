import { Service, SequelizeServiceOptions } from 'feathers-sequelize';
import { Application } from '../../declarations';
import { WorldModel } from '../../models/world.model';
import { randInt } from '../../util';

export class World extends Service {
  //eslint-disable-next-line @typescript-eslint/no-unused-vars
  constructor(options: Partial<SequelizeServiceOptions>, app: Application) {
    super(options);
  }

  async findRandom(): Promise<WorldModel> {
    return this.get(randInt());
  }

  async findMultiple(count: number): Promise<WorldModel[]> {
    const worldPromises: Promise<WorldModel>[] = [];

    for (let i = 0 ; i < count ; i++) {
      worldPromises.push(this.findRandom());
    }

    return await Promise.all(worldPromises);
  }

  async updateMultiple(count: number): Promise<WorldModel[]> {
    const worlds: WorldModel[] = [];

    for (let i = 0; i < count; i++) {
      const world = await this.findRandom();
      world.randomnumber = randInt();
      worlds.push(world);
      this.Model.update({ randomnumber: world.randomnumber }, {
        where: {
          id: world.id
        }
      });
    }

    return await Promise.all(worlds);
  }
}
