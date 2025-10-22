// Initializes the `plaintext` service on path `/plaintext`
import { ServiceAddons } from '@feathersjs/feathers';
import { Application } from '../../declarations';
import { Request, Response } from 'express';

// Add this service to the service type index
declare module '../../declarations' {
  interface ServiceTypes {
    'plaintext': ServiceAddons<any>;
  }
}

export default function (app: Application): void {
  app.get('/plaintext', (req: Request, res: Response) => {
    res.setHeader('Content-Type', 'text/plain');
    res.send('Hello, World!');
  });
}
