require "../spec_helper"

struct ParamsWithDefaultValue
  include HTTP::Params::Serializable

  getter required : Int32
  getter optional : Int32 = 42
end

describe ParamsWithDefaultValue do
  it do
    v = ParamsWithDefaultValue.from_query("required=41")
    v.required.should eq 41
    v.optional.should eq 42
  end

  it do
    v = ParamsWithDefaultValue.from_query("required=41&optional=43")
    v.required.should eq 41
    v.optional.should eq 43
  end
end
