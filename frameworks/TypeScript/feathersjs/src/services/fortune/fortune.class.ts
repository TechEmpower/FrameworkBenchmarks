import { Service, SequelizeServiceOptions } from 'feathers-sequelize';
import { Application } from '../../declarations';
import { FortuneModel } from '../../models/fortune.model';

export class Fortune extends Service {
  //eslint-disable-next-line @typescript-eslint/no-unused-vars
  constructor(options: Partial<SequelizeServiceOptions>, app: Application) {
    super(options);
  }

  async getFortunes() {
    const fortunes = <FortuneModel[]> await this.find();

    fortunes.push({
      id: 0,
      message: 'Additional fortune added at request time.',
    });

    fortunes.sort((f, s) => (f.message < s.message ? -1 : 1));

    return fortunes;
  }
}
