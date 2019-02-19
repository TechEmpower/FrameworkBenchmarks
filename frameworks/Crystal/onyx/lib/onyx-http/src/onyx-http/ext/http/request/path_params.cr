require "http/request"

class HTTP::Request
  # Each request instance initially has this empty hash object reference as *path_params*.
  # It helps to avoid allocating a new Hash instance every time.
  @@empty_params = Hash(String, String).new

  # A hash containing path params (extracted from the request's path).
  # It's automatically set when routing with `Onyx::REST::Router`, empty by default.
  #
  # For example, request with path `"/user/42/edit"` is
  # routed with `put "/user/:id/edit"` will have `{"id" => "42"}` path params.
  #
  # NOTE: You must not modify this hash values directly (e.g. `req.path_params["foo"] = "bar"`).
  # TODO: Make this hash immutable.
  property path_params : Hash(String, String) = @@empty_params
end
