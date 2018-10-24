import * as express from "express";
import * as Knex from "knex";
import { Model } from "objection";
import { Server } from "typescript-rest";

import Knexfile from "./config/Knexfile";
import defaultTo from "./helpers/defaultTo";

import Plaintext from "./controllers/plaintext";
import Json from "./controllers/json";
import SingleQuery from "./controllers/db";
import MultipleQueries from "./controllers/queries";
import DataUpdates from "./controllers/updates";

const DEFAULT_PORT = 3000;
// @ts-ignore - process.env.PORT may be undefined, and
// that's the point.
const PORT = defaultTo(DEFAULT_PORT, +process.env.PORT);

export default class ApiServer {
  private readonly app: express.Application;

  constructor() {
    this.app = express();
    this.app.set("etag", false);         // unsets the defaulted Etag header
    this.app.set("x-powered-by", false); // unsets the defaulted X-Powered-By header

    this.config();

    Server.buildServices(
      this.app,
      Plaintext,
      Json,
      SingleQuery,
      MultipleQueries,
      DataUpdates
    );
  }

  private config(): void {
    // Sets the global header `Server` as a middleware. We
    // are intentionally receiving and ignoring the `req`
    // parameter, indicated by the underscore.
    this.app.use((_, res, next): void => {
      res.set("Server", "TypeScript-rest");

      next();
    });

    // Initiatlize connection to the database and connect
    // the knex query builder to our objection models.
    const knex = Knex(Knexfile);
    Model.knex(knex);
  }

  start(): void {
    this.app.listen(PORT, (err: any) => {
      if (err) {
        throw err;
      }

      console.info(`Server listening on port ${PORT}`);
    });
  }
}
