import { GET, Path, QueryParam } from "typescript-rest";

import randomNumber from "../helpers/randomNumber";
import sanitizeQueries from "../helpers/sanitizeQueries";

import World from "../models/world";

@Path("/queries")
export default class MultipleQueries {
  /**
   * Implements the `Multiple queries` test type.
   */

  @GET
  async multipleQueries(@QueryParam("queries") queries: string): Promise<World[]> {
    const length: number = sanitizeQueries(queries);
    const ids: number[] = [];

    // Use a for-loop here because Array.from is just not
    // performant at all, but is really nice.
    // https://jsbench.me/ntjn3s2t0y/1
    for (let i = 0; i < length; i += 1) {
      ids.push(randomNumber());
    }

    const worlds: World[] = await World
      .query()
      .findByIds(ids);

    return worlds;
  }
}
