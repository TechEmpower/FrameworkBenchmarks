require "../spec_helper"
require "../../src/http-params-serializable/ext/time/epoch_converter"

# Used to test the converter feature
struct TimeParams
  include HTTP::Params::Serializable

  @[HTTP::Param(converter: Time::EpochConverter)]
  getter epoch_time : Time

  @[HTTP::Param(converter: Time::EpochConverter)]
  getter nilable_epoch_time : Time?

  @[HTTP::Param(converter: Time::EpochConverter)]
  getter array_epoch_time : Array(Time)

  @[HTTP::Param(converter: Time::EpochConverter)]
  getter nilable_array_epoch_time : Array(Time)?

  @[HTTP::Param(converter: Time::EpochConverter)]
  getter array_nilable_epoch_time : Array(Time?)

  @[HTTP::Param(converter: Time::EpochConverter)]
  getter nilable_array_nilable_epoch_time : Array(Time?)?
end

describe TimeParams do
  it do
    v = TimeParams.from_query("epoch_time=1544958806&nilableEpochTime=1544958806&ArrayEpochTime[]=1544958806&nilable-array-epoch-time[0]=1544958806&Nilable-Array-Nilable-Epoch-Time[]=1544958806&array_nilable_epoch_time[0]=")

    v.epoch_time.should eq Time.unix(1544958806)
    v.nilable_epoch_time.should eq Time.unix(1544958806)
    v.array_epoch_time.should eq [Time.unix(1544958806)]
    v.nilable_array_epoch_time.should eq [Time.unix(1544958806)]
    v.array_nilable_epoch_time.should eq [nil]
    v.nilable_array_nilable_epoch_time.should eq [Time.unix(1544958806)]

    v.to_query.should eq escape("epochTime=1544958806&nilableEpochTime=1544958806&arrayEpochTime[]=1544958806&nilableArrayEpochTime[]=1544958806&nilableArrayNilableEpochTime[]=1544958806")
  end

  it "raises on type mismatch" do
    assert_raise(
      TimeParams,
      "epoch_time=foo",
      HTTP::Params::Serializable::ParamTypeCastError,
      "Parameter \"epoch_time\" cannot be cast from \"foo\" to Time",
      ["epoch_time"],
    )

    assert_raise(
      TimeParams,
      "epoch_time=1544958806&nilable_epoch_time=foo",
      HTTP::Params::Serializable::ParamTypeCastError,
      "Parameter \"nilable_epoch_time\" cannot be cast from \"foo\" to (Time | Nil)",
      ["nilable_epoch_time"],
    )

    assert_raise(
      TimeParams,
      "epoch_time=1544958806&array_epoch_time[]=foo",
      HTTP::Params::Serializable::ParamTypeCastError,
      "Parameter \"array_epoch_time[]\" cannot be cast from \"foo\" to Time",
      ["array_epoch_time", ""],
    )

    assert_raise(
      TimeParams,
      "epoch_time=1544958806&array_epoch_time[]=1544958806&nilable_array_nilable_epoch_time[]=foo",
      HTTP::Params::Serializable::ParamTypeCastError,
      "Parameter \"nilable_array_nilable_epoch_time[]\" cannot be cast from \"foo\" to (Time | Nil)",
      ["nilable_array_nilable_epoch_time", ""],
    )
  end
end
