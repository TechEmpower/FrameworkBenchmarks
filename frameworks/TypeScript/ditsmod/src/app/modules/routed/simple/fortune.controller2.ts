import Handlebars from 'handlebars';
import { RequestContext, controller, route } from '@ditsmod/core';

import { additionalFortune, compare } from '#utils/helper.js';
import { DbService } from '#service/db/db.service.js';

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
export class FortuneController2 {
  constructor(private dbService: DbService) {}

  @route('GET', 'fortunes2')
  async fortunes(ctx: RequestContext) {
    const fortunes = await this.dbService.findAllFortunes();
    fortunes.push(additionalFortune);
    fortunes.sort(compare);
    ctx.nodeRes.setHeader('Server', 'Ditsmod');
    ctx.nodeRes.setHeader('Content-Type', 'text/html; charset=utf-8');
    ctx.nodeRes.end(tmpl({ fortunes }));
  }
}
