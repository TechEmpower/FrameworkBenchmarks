import { AnyObj, controller, RequestContext, SingletonRequestContext, route } from '@ditsmod/core';
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

@controller({ isSingleton: true })
export class OneController {
  constructor(private dbService: DbService) {}

  @route('GET', 'db')
  async getSingleQuery(ctx: RequestContext) {
    const id = getRandomNumber();
    const result = await this.dbService.findOneWorld(id);
    this.sendJson(ctx, result);
  }

  @route('GET', 'queries')
  async getMultiQueries(ctx: SingletonRequestContext) {
    const result = await this.dbService.getMultiQueries(ctx.queryParams!.queries);
    this.sendJson(ctx, result);
  }

  @route('GET', 'cached-queries')
  async getCachedWorlds(ctx: SingletonRequestContext) {
    const result = await this.dbService.getMultiQueries(ctx.queryParams!.count, false);
    this.sendJson(ctx, result);
  }

  @route('GET', 'updates')
  async getUpdates(ctx: SingletonRequestContext) {
    const worlds = await this.dbService.saveWorlds(ctx.queryParams!.queries);
    this.sendJson(ctx, worlds);
  }

  @route('GET', 'fortunes')
  async fortunes(ctx: RequestContext) {
    const fortunes = await this.dbService.findAllFortunes();
    fortunes.push(additionalFortune);
    fortunes.sort(compare);
    ctx.nodeRes.setHeader('Server', 'Ditsmod');
    ctx.nodeRes.setHeader('Content-Type', 'text/html; charset=utf-8');
    ctx.nodeRes.end(tmpl({ fortunes }));
  }

  @route('GET', 'plaintext')
  getHello(ctx: SingletonRequestContext) {
    ctx.nodeRes.setHeader('Server', 'Ditsmod');
    ctx.nodeRes.setHeader('Content-Type', 'text/plain; charset=utf-8');
    ctx.nodeRes.end('Hello, World!');
  }

  @route('GET', 'json')
  getJson(ctx: SingletonRequestContext) {
    ctx.nodeRes.setHeader('Server', 'Ditsmod');
    ctx.nodeRes.setHeader('Content-Type', 'application/json; charset=utf-8');
    ctx.nodeRes.end(JSON.stringify({ message: 'Hello, World!' }));
  }

  protected sendJson(ctx: RequestContext, value: AnyObj) {
    ctx.nodeRes.setHeader('Server', 'Ditsmod');
    ctx.nodeRes.setHeader('Content-Type', 'application/json; charset=utf-8');
    ctx.nodeRes.end(JSON.stringify(value));
  }
}
