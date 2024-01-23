import { Application } from '../declarations';

import plaintext from './plaintext/plaintext.service';

import json from './json/json.service';

import fortune from './fortune/fortune.service';

import world from './world/world.service';

// Don't remove this comment. It's needed to format import lines nicely.

export default function (app: Application): void {
  app.configure(plaintext);
  app.configure(json);
  app.configure(fortune);
  app.configure(world);
}
