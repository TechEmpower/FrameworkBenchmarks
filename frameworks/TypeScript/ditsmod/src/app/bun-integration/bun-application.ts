import { AnyFn, AppInitializer, Application } from '@ditsmod/core';
import { Serve, Server } from 'bun';

export class BunApplication extends Application {
  protected override createServer(requestListener: any): any {
    const serveOptions = this.appOptions.serverOptions as Serve;
    serveOptions.fetch ??= (req) => requestListener(req);
    return Bun.serve(serveOptions);
  }

  protected override async createServerAndBindToListening(appInitializer: AppInitializer, resolve: AnyFn) {
    this.flushLogs();
    const server = (await this.createServer(appInitializer.requestListener)) as Server;
    this.systemLogMediator.serverListen(this, server.hostname, server.port);
    resolve({ server });
  }
}
