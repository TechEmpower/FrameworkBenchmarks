import { PreRouter } from '@ditsmod/core';
import { NodeRes } from './node-res.js';

export class BunPreRouter extends PreRouter {
  override requestListener: any = async (req: Request) => {
    const nodeReq = req as any;
    const nodeRes = new NodeRes();

    const url = new URL(req.url);
    const uri = url.pathname;
    const queryString = url.search.slice(1);
    const { handle, params } = this.router.find(req.method as any, uri);
    if (!handle) {
      this.sendNotImplemented(nodeRes as any);
      const body = await nodeRes.body;
      return new Response(body, nodeRes);
    }

    await handle(nodeReq, nodeRes as any, params!, queryString).catch((err) => {
      this.sendInternalServerError(nodeRes as any, err);
    });

    const body = await nodeRes.body;
    return new Response(body, nodeRes);
  };
}
