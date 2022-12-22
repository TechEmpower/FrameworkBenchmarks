open Opium
open Lwt.Syntax

(*
 * bin/routes contains a mapping between
 * Opium request -> response
 *
 * lib/routes contains the actual logic for
 * db calls and etc.
 *)

let plaintext _req =
  let text = Opi.Routes.plaintext () in
  Lwt.return @@ Response.of_plain_text text

let json _req =
  let json = Opi.Routes.json () in
  Lwt.return @@ Response.of_json json

let single_query _req =
  let+ json = Opi.Routes.single_query () in
  Response.of_json json

let get_count_param req = match (int_of_string (Router.param req "count")) with
  | x -> x
  | exception _e -> 1

let multiple_queries req =
  let count = get_count_param req in
  let+ json = Opi.Routes.multiple_queries count in
  Response.of_json json

let updates req =
  let count = get_count_param req in
  let+ json = Opi.Routes.updates count in
  Response.of_json json

let fortunes _req =
  let+ html = Opi.Routes.fortunes () in
  Response.of_html ~indent:false html

