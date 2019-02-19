require "../spec_helper"

struct NestedParams
  include HTTP::Params::Serializable
  getter nested : Nested?

  struct Nested
    include HTTP::Params::Serializable

    getter required : Bool
    getter required_array : Array(Int32)
    getter subnested : Array(Subnested)

    struct Subnested
      include HTTP::Params::Serializable
      getter optional : String?
    end
  end
end

describe NestedParams do
  describe "(sub)nested" do
    it do
      v = NestedParams.from_query("nested[required]=true&nested[required_array][]=42&nested[subnested][0][unknown]=foo")

      v.nested.should be_a(NestedParams::Nested)
      nested = v.nested.not_nil!

      nested.required.should eq true
      nested.required_array.should eq [42]

      nested.subnested.size.should eq 1
      nested.subnested.first.should be_a(NestedParams::Nested::Subnested)
      subnested = nested.subnested.first.not_nil!

      subnested.optional.should be_nil

      # Subnested is not nil itself, but it's not rendered due to being empty
      v.to_query.should eq escape("nested[required]=true&nested[requiredArray][]=42")
    end

    # The keys are checked de-facto, thus "nested[]=" key is never validated, therefore skipped
    # In this case, "required" param is required, so it raises
    #
    # RFC: Improve this behaviour?
    it "skips array indices on a nested object param" do
      assert_raise(
        NestedParams,
        "nested[]=true",
        HTTP::Params::Serializable::ParamMissingError,
        "Parameter \"nested[required]\" is missing",
        ["nested", "required"]
      )
    end

    it "raises on empty index for nested content" do
      # Offending: nested[subnested][][optional]=foo
      #                             ^
      assert_raise(
        NestedParams,
        "nested[required]=true&nested[required_array][]=42&nested[subnested][][optional]=foo",
        HTTP::Params::Serializable::EmptyIndexForNonScalarArrayParamError,
        "Parameter \"nested[subnested]\" is a non-scalar array, but has empty index at \"nested[subnested][][optional]=\"",
        ["nested", "subnested", ""]
      )
    end

    it "raises on explicit keys" do
      # Offending: nested=foo
      assert_raise(
        NestedParams,
        "nested=foo",
        HTTP::Params::Serializable::ExplicitKeyForNonScalarParam,
        "Parameter \"nested\" is of type Object, but set explicitly with \"nested=\"",
        ["nested"]
      )

      # Offending: nested[subnested]=foo
      assert_raise(
        NestedParams,
        "nested[required]=true&nested[required_array][]=42&nested[required_array][]=43&nested[subnested]=foo",
        HTTP::Params::Serializable::ExplicitKeyForNonScalarParam,
        "Parameter \"nested[subnested]\" is of type Array, but set explicitly with \"nested[subnested]=\"",
        ["nested", "subnested"]
      )

      # Offending: nested[subnested][0]=foo
      assert_raise(
        NestedParams,
        "nested[required]=true&nested[required_array][]=42&nested[required_array][0]=42&nested[subnested][0]=foo",
        HTTP::Params::Serializable::ExplicitKeyForNonScalarParam,
        "Parameter \"nested[subnested][0]\" is of type Object, but set explicitly with \"nested[subnested][0]=\"",
        ["nested", "subnested", "0"]
      )
    end
  end
end
