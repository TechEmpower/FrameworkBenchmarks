import { QueryBuilder } from "objection";
import { GET, Path, QueryParam } from "typescript-rest";

import randomNumber from "../helpers/randomNumber";
import sanitizeQueries from "../helpers/sanitizeQueries";

import World from "../models/world";

@Path("/updates")
export default class DataUpdates {
  /**
   * Implements the `Data updates` test type.
   */

  @GET
  async dataUpdates(@QueryParam("queries") queries: string): Promise<World[]> {
    const length: number = sanitizeQueries(queries);
    const worlds: QueryBuilder<World, World, World>[] = [];

    for (let i = 0; i < length; i += 1) {
      const id: number = randomNumber();
      worlds.push(
        World
          .query()
          .patch({ randomnumber: randomNumber() })
          .findById(id)
          .returning("*")
          .throwIfNotFound()
      );
    }

    return Promise.all(worlds);
  }
}
