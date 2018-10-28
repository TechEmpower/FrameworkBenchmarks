import { GET, Path } from "typescript-rest";

import randomNumber from "../helpers/randomNumber";
import World from "../models/world";

@Path("/db")
export default class SingleQuery {
  /**
   * Implements the `Single query` test type.
   */

  @GET
  async singleQuery(): Promise<World> {
    const id: number = randomNumber();
    const world: World = await World
      .query()
      .findById(id)
      .throwIfNotFound();

    return world;
  }
}
