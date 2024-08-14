import { Writable } from 'node:stream';

export class NodeRes extends Writable {
  #chunks: Buffer[] = [];
  #resolve: (body: string) => void;
  status: number = 200;
  headers = {} as HeadersInit;
  body = new Promise<any>((resolve) => (this.#resolve = resolve));
  headersSent?: boolean;
  statusText?: string;

  set statusCode(statusCode: number) {
    this.status = statusCode;
  }

  getHeader(name: string) {
    return this.headers[name as keyof HeadersInit];
  }

  getHeaders() {
    return this.headers;
  }

  setHeader(name: string, value: number | string | readonly string[]) {
    this.headers = { ...this.headers, [name]: value } as HeadersInit;
    return this;
  }

  writeHead(statusCode: number, headers?: HeadersInit): this;
  writeHead(statusCode: number, statusMessage: string, headers?: HeadersInit): this;
  writeHead(statusCode: number, statusMsgOrHeaders?: string | HeadersInit, headers?: HeadersInit): this {
    this.status = statusCode;
    if (typeof statusMsgOrHeaders == 'object') {
      this.mergeHeaders(statusMsgOrHeaders);
    } else {
      this.statusText = statusMsgOrHeaders;
      this.mergeHeaders(headers);
    }
    return this;
  }

  override _write(chunk: any, encoding: BufferEncoding, callback: (error?: Error | null) => void): void {
    this.#chunks.push(Buffer.from(chunk));
    callback();
  }

  override _final(callback: (error?: Error | null) => void): void {
    const finalData = Buffer.concat(this.#chunks);
    this.headersSent = true;
    this.#resolve(finalData.toString());
    callback();
  }

  protected mergeHeaders(headers: HeadersInit = {}) {
    if (Array.isArray(headers)) {
      headers.forEach(([key, val]) => ((this.headers as any)[key] = val));
    } else {
      this.headers = { ...this.headers, ...headers };
    }
  }
}
