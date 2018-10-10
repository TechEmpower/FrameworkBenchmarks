import { GET, Path } from "typescript-rest";

@Path("/json")
export default class Json {
  /**
   * Sends a json message
   */

  @GET
  json(): string {
    return JSON.stringify({ message: "Hello, World!" });
  }
}
