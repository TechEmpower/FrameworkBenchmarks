import { Handlers } from "./../../handlers.ts";
import FortunesHandler from "./fortunes.ts";
import MultipleQueriesHandler from "./multiple-queries.ts";
import SingleQueryHandler from "./single-query.ts";
import UpdatesHandler from "./updates.ts";

export const MongoRawHandlers = {
  "/db": SingleQueryHandler,
  "/queries": MultipleQueriesHandler,
  "/updates": UpdatesHandler,
  "/fortunes": FortunesHandler,
} as Handlers;
