require "../spec_helper"
require "../../src/http-params-serializable/ext/uri"

# Used to test explict Scalar objects
# Note that any value is considered valid for an URI
struct URIParams
  include HTTP::Params::Serializable

  getter uri : URI
  getter nilable_uri : URI?
  getter array_uri : Array(URI)
  getter nilable_array_uri : Array(URI)?
  getter array_nilable_uri : Array(URI?)
  getter nilable_array_nilable_uri : Array(URI?)?
end

describe URIParams do
  it do
    v = URIParams.from_query("uri=https://example.com&nilable_uri=foo&arrayUri[0]=bar&nilableArrayUri[]=baz&arrayNilableUri[]=&nilableArrayNilableUri[0]=")
    v.uri.should eq URI.parse("https://example.com")
    v.nilable_uri.should eq URI.parse("foo")
    v.array_uri.should eq [URI.parse("bar")]
    v.nilable_array_uri.should eq [URI.parse("baz")]
    v.array_nilable_uri.should eq [nil]
    v.nilable_array_nilable_uri.should eq [nil]

    v.to_query.should eq escape("uri=https://example.com&nilableUri=foo&arrayUri[]=bar&nilableArrayUri[]=baz")
  end
end
