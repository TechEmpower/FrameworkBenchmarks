require "../spec_helper"

struct TestView
  include Onyx::REST::View

  def initialize(@foo : String, @bar : Int32? = nil)
  end

  json do
    object do
      field "foo", @foo
      field "bar", @bar
    end
  end

  text do
    "foo: #{@foo}, bar: #{@bar}"
  end
end

describe Onyx::REST::View do
  view = TestView.new("baz", 42)

  it "has #to_json" do
    view.to_json.should eq %Q[{"foo":"baz","bar":42}]
  end

  it "has #to_json(builder)" do
    io = IO::Memory.new
    builder = JSON::Builder.new(io)

    builder.document do
      view.to_json(builder)
    end

    io.to_s.should eq %Q[{"foo":"baz","bar":42}]
  end

  it "has #to_text" do
    view.to_text.should eq "foo: baz, bar: 42"
  end

  it "has #to_text(io)" do
    io = IO::Memory.new
    view.to_text(io)
    io.to_s.should eq "foo: baz, bar: 42"
  end
end
