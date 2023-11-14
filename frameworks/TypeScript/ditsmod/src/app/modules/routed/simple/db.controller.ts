import { AnyObj, controller, inject, QUERY_PARAMS, Res, route } from '@ditsmod/core';

import { DbService } from '#service/db/db.service.js';
import { getRandomNumber } from '#utils/helper.js';

@controller()
export class DbController {
  constructor(
    private res: Res,
    private dbService: DbService,
  ) {
    res.nodeRes.setHeader('Server', 'Ditsmod');
  }

  @route('GET', 'db')
  async getSingleQuery() {
    const id = getRandomNumber();
    const result = await this.dbService.findOneWorld(id);
    this.res.sendJson(result);
  }

  @route('GET', 'queries')
  async getMultiQueries(@inject(QUERY_PARAMS) queryParams: AnyObj) {
    const result = await this.dbService.getMultiQueries(queryParams.queries);
    this.res.sendJson(result);
  }

  @route('GET', 'cached-queries')
  async getCachedWorlds(@inject(QUERY_PARAMS) queryParams: AnyObj) {
    const result = await this.dbService.getMultiQueries(queryParams.count, false);
    this.res.sendJson(result);
  }

  @route('GET', 'updates')
  async getUpdates(@inject(QUERY_PARAMS) queryParams: AnyObj) {
    const worlds = await this.dbService.saveWorlds(queryParams.queries);
    this.res.sendJson(worlds);
  }
}
