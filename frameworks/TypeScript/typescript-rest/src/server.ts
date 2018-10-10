import * as express from "express";
import { Server } from "typescript-rest";

import defaultTo from "./helpers/defaultTo";

import Plaintext from "./controllers/plaintext";
import Json from "./controllers/json";

const DEFAULT_PORT = 3000;
const PORT = defaultTo(DEFAULT_PORT, +process.env.PORT);

export default class ApiServer {
  private readonly app: express.Application;

  constructor() {
    this.app = express();

    Server.buildServices(
      this.app,
      Plaintext,
      Json
    );
  }

  start() {
    this.app.listen(PORT, (err: any) => {
      if (err) {
        throw err;
      }

      console.info(`Server listening on port ${PORT}`);
    });
  }
}
