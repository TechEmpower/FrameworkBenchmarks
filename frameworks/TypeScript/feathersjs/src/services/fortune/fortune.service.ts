// Initializes the `fortune` service on path `/fortune`
import { ServiceAddons } from '@feathersjs/feathers';
import { Application } from '../../declarations';
import { Request, Response } from 'express';
import { Fortune } from './fortune.class';
import createModel from '../../models/fortune.model';
import hooks from './fortune.hooks';

// Add this service to the service type index
declare module '../../declarations' {
  interface ServiceTypes {
    'fortune': Fortune & ServiceAddons<any>;
  }
}

export default function (app: Application): void {
  const options = {
    Model: createModel(app)
  };

  // Initialize our service with any options it requires
  app.use('/fortune', new Fortune(options, app));

  app.get('/fortunes', async (req: Request, res: Response) => {
    const fortunes = await app.service('fortune').getFortunes();
    res.render('fortunes', { fortunes });
  });

  // Get our initialized service so that we can register hooks
  const service = app.service('fortune');

  service.hooks(hooks);
}
