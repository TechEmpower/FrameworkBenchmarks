import { controller, Res, route } from '@ditsmod/core';

@controller()
export class WithoutDbController {
  constructor(private res: Res) {
    res.nodeRes.setHeader('Server', 'Ditsmod');
  }

  @route('GET', 'plaintext')
  getHello() {
    this.res.send('Hello, World!');
  }

  @route('GET', 'json')
  getJson() {
    this.res.sendJson({ message: 'Hello, World!' });
  }
}
