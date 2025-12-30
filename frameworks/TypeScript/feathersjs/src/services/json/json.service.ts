// Initializes the `json` service on path `/json`
import { ServiceAddons } from '@feathersjs/feathers';
import { Application } from '../../declarations';
import { Request, Response } from 'express';

// Add this service to the service type index
declare module '../../declarations' {
  interface ServiceTypes {
    'json': ServiceAddons<any>;
  }
}

export default function (app: Application): void {
  app.get('/json', (req: Request, res: Response) => {
    res.json({ message: 'Hello, World!' });
  });
}
