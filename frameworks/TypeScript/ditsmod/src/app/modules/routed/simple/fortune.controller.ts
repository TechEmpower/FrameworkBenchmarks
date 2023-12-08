import Handlebars from 'handlebars';
import { NODE_RES, NodeResponse, controller, inject, route } from '@ditsmod/core';

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

@controller()
export class FortuneController {
  @route('GET', 'fortunes')
  async fortunes(@inject(NODE_RES) nodeRes: NodeResponse, dbService: DbService) {
    const fortunes = await dbService.findAllFortunes();
    fortunes.push(additionalFortune);
    fortunes.sort(compare);
    nodeRes.setHeader('Server', 'Ditsmod');
    nodeRes.setHeader('Content-Type', 'text/html; charset=utf-8');
    nodeRes.end(tmpl({ fortunes }));
  }
}
