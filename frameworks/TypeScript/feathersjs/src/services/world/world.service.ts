// Initializes the `world` service on path `/world`
import { Params, ServiceAddons } from '@feathersjs/feathers';
import { Application } from '../../declarations';
import { Request, Response } from 'express';
import { World } from './world.class';
import createModel from '../../models/world.model';
import hooks from './world.hooks';
import { randInt } from '../../util';

// Add this service to the service type index
declare module '../../declarations' {
  interface ServiceTypes {
    'world': World & ServiceAddons<any>;
  }
}

export default function (app: Application): void {
  const options = {
    Model: createModel(app),
    paginate: app.get('paginate')
  };

  // Initialize our service with any options it requires
  app.use('/world', new World(options, app));

  app.get('/db', async (req: Request, res: Response) => {
    const world = await app.service('world').findRandom();

    res.json(world);
  });

  app.get('/queries', async (req: Request, res: Response) => {
    const queries = Math.min(Math.max(parseInt(<string>req.query.queries) || 1, 1), 500);
    const worlds = await app.service('world').findMultiple(queries);
    
    res.json(worlds);
  });

  app.get('/update', async (req: Request, res: Response) => {
    const queries = Math.min(Math.max(parseInt(<string>req.query.queries) || 1, 1), 500);
    const worlds = await app.service('world').updateMultiple(queries);
    
    res.json(worlds);
  });

  // Get our initialized service so that we can register hooks
  const service = app.service('world');

  service.hooks(hooks);
}
