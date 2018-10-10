import { GET, Path } from "typescript-rest";

interface IResult {
  message: string;
}

@Path("/json")
export default class Json {
  /**
   * Sends a json message. Under the hood, Express should
   * have serialized the result with `res.json`.
   */

  @GET
  json(): IResult {
    return { message: "Hello, World!" };
  }
}
