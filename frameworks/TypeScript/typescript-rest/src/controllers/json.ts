import { GET, Path } from "typescript-rest";

interface IResult {
  message: string;
}

@Path("/json")
export default class Json {
  /**
<<<<<<< HEAD
<<<<<<< HEAD
   * Implements the `JSON Serialization` test type. Under
   * the hood, Express should have serialized the result
   * with `res.json`.
=======
   * Sends a json message. Under the hood, Express should
   * have serialized the result with `res.json`.
>>>>>>> Wraps up initial server configuration
=======
   * Implements the `JSON Serialization` test type. Under
   * the hood, Express should have serialized the result
   * with `res.json`.
>>>>>>> Finishes framework implementation, minus fortunes
   */

  @GET
  json(): IResult {
    return { message: "Hello, World!" };
  }
}
