require "../spec_helper"

struct Params
  include HTTP::Params::Serializable

  getter required : Int32

  @[HTTP::Param(key: "fooBar")]
  getter optional : Bool | Char | String | Int32 | Float32 | Nil
end

describe Params do
  describe "required" do
    v = Params.from_query("required=42&unknown=foo")

    describe "parsing" do
      it do
        v.required.should be_a(Int32)
        v.required.should eq 42
      end

      it "raises on missing" do
        assert_raise(
          Params,
          "unknown=foo",
          HTTP::Params::Serializable::ParamMissingError,
          "Parameter \"required\" is missing",
          ["required"]
        )
      end

      it "raises on empty" do
        assert_raise(
          Params,
          "required=&unknown=foo",
          HTTP::Params::Serializable::ParamMissingError,
          "Parameter \"required\" is missing",
          ["required"]
        )
      end

      it "raises on type mismatch" do
        assert_raise(
          Params,
          "required=foo",
          HTTP::Params::Serializable::ParamTypeCastError,
          "Parameter \"required\" cannot be cast from \"foo\" to Int32",
          ["required"],
        )
      end
    end

    describe "serializing" do
      it do
        v.to_query.should eq "required=42"
      end
    end
  end

  describe "optional" do
    it "casts to Bool" do
      v = Params.from_query("required=42&fooBar=true")
      v.optional.should be_a(Bool)
      v.optional.should eq true
      v.to_query.should eq "required=42&fooBar=true"
    end

    it "casts to Char" do
      v = Params.from_query("required=42&fooBar=t")
      v.optional.should be_a(Char)
      v.optional.should eq 't'
      v.to_query.should eq "required=42&fooBar=t"
    end

    it "casts to String" do
      v = Params.from_query("required=42&fooBar=foo")
      v.optional.should be_a(String)
      v.optional.should eq "foo"
      v.to_query.should eq "required=42&fooBar=foo"
    end

    it "casts to Float32 instead of Int32" do
      v = Params.from_query("required=42&fooBar=42")
      v.optional.should be_a(Float32)
      v.optional.should eq 42.0
      v.to_query.should eq "required=42&fooBar=42.0"
    end

    it "casts to Float32" do
      v = Params.from_query("required=42&fooBar=-42.1")
      v.optional.should be_a(Float32)
      v.optional.should eq -42.1_f32
      v.to_query.should eq "required=42&fooBar=-42.1"
    end

    it "stays nil on empty" do
      v = Params.from_query("required=42&fooBar=")
      v.optional.should be_nil
      v.to_query.should eq "required=42"
    end

    it "stays nil on missing" do
      v = Params.from_query("required=42")
      v.optional.should be_nil
      v.to_query.should eq "required=42"
    end
  end
end
