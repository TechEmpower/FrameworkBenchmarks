import { GET, Path } from "typescript-rest";

@Path("/plaintext")
export default class Plaintext {
  /**
   * Sends the plaintext message
   */

  @GET
  plaintext(): string {
    return "Hello, World!";
  }
}
