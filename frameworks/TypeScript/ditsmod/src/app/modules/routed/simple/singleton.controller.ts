import { controller, route, SingletonRequestContext } from '@ditsmod/core';

@controller({ isSingleton: true })
export class SingletonController {
  @route('GET', 'plaintext2')
  getHello(ctx: SingletonRequestContext) {
    ctx.nodeRes.setHeader('Server', 'Ditsmod');
    ctx.nodeRes.setHeader('Content-Type', 'text/plain; charset=utf-8');
    ctx.nodeRes.end('Hello, World!');
  }

  @route('GET', 'json2')
  getJson(ctx: SingletonRequestContext) {
    ctx.nodeRes.setHeader('Server', 'Ditsmod');
    ctx.nodeRes.setHeader('Content-Type', 'application/json; charset=utf-8');
    ctx.nodeRes.end(JSON.stringify({ message: 'Hello, World!' }));
  }
}
