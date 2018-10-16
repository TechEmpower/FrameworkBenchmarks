import { GET, Path } from "typescript-rest";

interface IResult {
  message: string;
}

@Path("/json")
export default class Json {
  /**
   * Implements the `JSON Serialization` test type. Under
   * the hood, Express should have serialized the result
   * with `res.json`.
   */

  @GET
  json(): IResult {
    return { message: "Hello, World!" };
  }
}
