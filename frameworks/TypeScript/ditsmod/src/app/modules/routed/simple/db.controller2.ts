import { AnyObj, controller, RequestContext, SingletonRequestContext, route } from '@ditsmod/core';

import { DbService } from '#service/db/db.service.js';
import { getRandomNumber } from '#utils/helper.js';

@controller({ isSingleton: true })
export class DbController2 {
  constructor(private dbService: DbService) {}

  @route('GET', 'db2')
  async getSingleQuery(ctx: RequestContext) {
    const id = getRandomNumber();
    const result = await this.dbService.findOneWorld(id);
    this.sendJson(ctx, result);
  }

  @route('GET', 'queries2')
  async getMultiQueries(ctx: SingletonRequestContext) {
    const result = await this.dbService.getMultiQueries(ctx.queryParams!.queries);
    this.sendJson(ctx, result);
  }

  @route('GET', 'cached-queries2')
  async getCachedWorlds(ctx: SingletonRequestContext) {
    const result = await this.dbService.getMultiQueries(ctx.queryParams!.count, false);
    this.sendJson(ctx, result);
  }

  @route('GET', 'updates2')
  async getUpdates(ctx: SingletonRequestContext) {
    const worlds = await this.dbService.saveWorlds(ctx.queryParams!.queries);
    this.sendJson(ctx, worlds);
  }

  protected sendJson(ctx: RequestContext, value: AnyObj) {
    ctx.nodeRes.setHeader('Server', 'Ditsmod');
    ctx.nodeRes.setHeader('Content-Type', 'application/json; charset=utf-8');
    ctx.nodeRes.end(JSON.stringify(value));
  }
}
