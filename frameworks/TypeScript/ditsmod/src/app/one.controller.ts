import { AnyObj, controller, RequestContext, optional } from '@ditsmod/core';
import { route } from '@ditsmod/routing';
import Handlebars from 'handlebars';

import { DbService } from './db.service.js';
import { additionalFortune, compare, getRandomNumber } from './helper.js';

const tmpl = Handlebars.compile(
  [
    '<!DOCTYPE html>',
    '<html>',
    '<head><title>Fortunes</title></head>',
    '<body>',
    '<table>',
    '<tr>',
    '<th>id</th>',
    '<th>message</th>',
    '</tr>',
    '{{#fortunes}}',
    '<tr>',
    '<td>{{id}}</td>',
    '<td>{{message}}</td>',
    '</tr>',
    '{{/fortunes}}',
    '</table>',
    '</body>',
    '</html>',
  ].join(''),
);

@controller({ scope: 'ctx' })
export class OneController {
  constructor(@optional() private dbService: DbService) {}

  @route('GET', 'db')
  async getSingleQuery(ctx: RequestContext) {
    const id = getRandomNumber();
    const result = await this.dbService.findOneWorld(id);
    this.sendJson(ctx, result);
  }

  @route('GET', 'queries')
  async getMultiQueries(ctx: RequestContext) {
    const result = await this.dbService.getMultiQueries(ctx.queryParams!.queries);
    this.sendJson(ctx, result);
  }

  @route('GET', 'cached-queries')
  async getCachedWorlds(ctx: RequestContext) {
    const result = await this.dbService.getMultiQueries(ctx.queryParams!.count, false);
    this.sendJson(ctx, result);
  }

  @route('GET', 'updates')
  async getUpdates(ctx: RequestContext) {
    const worlds = await this.dbService.saveWorlds(ctx.queryParams!.queries);
    this.sendJson(ctx, worlds);
  }

  @route('GET', 'fortunes')
  async fortunes(ctx: RequestContext) {
    const fortunes = await this.dbService.findAllFortunes();
    fortunes.push(additionalFortune);
    fortunes.sort(compare);
    ctx.rawRes.setHeader('Server', 'Ditsmod');
    ctx.rawRes.setHeader('Content-Type', 'text/html; charset=utf-8');
    ctx.rawRes.end(tmpl({ fortunes }));
  }

  @route('GET', 'plaintext')
  getHello(ctx: RequestContext) {
    ctx.rawRes.setHeader('Server', 'Ditsmod');
    ctx.rawRes.setHeader('Content-Type', 'text/plain; charset=utf-8');
    ctx.rawRes.end('Hello, World!');
  }

  @route('GET', 'json')
  getJson(ctx: RequestContext) {
    this.sendJson(ctx, { message: 'Hello, World!' });
  }

  protected sendJson(ctx: RequestContext, value: AnyObj) {
    ctx.rawRes.setHeader('Server', 'Ditsmod');
    ctx.rawRes.setHeader('Content-Type', 'application/json; charset=utf-8');
    ctx.rawRes.end(JSON.stringify(value));
  }
}
