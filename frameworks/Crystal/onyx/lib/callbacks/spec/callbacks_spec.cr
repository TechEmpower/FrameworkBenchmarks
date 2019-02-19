require "./spec_helper"

class Parent
  include Callbacks

  @buffer = [] of String
  getter buffer

  before { @buffer.push("parent before #1") }
  before { @buffer.push("parent before #2") }

  def action
    42
  end

  after { @buffer.push("parent after #1") }
  after { @buffer.push("parent after #2") }

  def call
    with_callbacks do
      @buffer.push("call")
      action
    end
  end
end

class Child < Parent
  before { @buffer.push("child before #1") }
  before { @buffer.push("child before #2") }

  after { @buffer.push("child after #1") }
  after { @buffer.push("child after #2") }
end

class SubChild < Child
  before { @buffer.push("subchild before #1") }
  before { @buffer.push("subchild before #2") }

  after { @buffer.push("subchild after #1") }
  after { @buffer.push("subchild after #2") }
end

describe Callbacks do
  it "work with Parent" do
    instance = Parent.new
    instance.call.should eq 42
    instance.buffer.should eq "
      parent before #1
      parent before #2
      call
      parent after #1
      parent after #2
      ".split("\n").map(&.strip).reject(&.empty?)
  end

  it "work with Child" do
    instance = Child.new
    instance.call.should eq 42
    instance.buffer.should eq "
      child before #1
      child before #2
      parent before #1
      parent before #2
      call
      parent after #1
      parent after #2
      child after #1
      child after #2
      ".split("\n").map(&.strip).reject(&.empty?)
  end

  it "work with SubChild" do
    instance = SubChild.new
    instance.call.should eq 42
    instance.buffer.should eq "
      subchild before #1
      subchild before #2
      child before #1
      child before #2
      parent before #1
      parent before #2
      call
      parent after #1
      parent after #2
      child after #1
      child after #2
      subchild after #1
      subchild after #2
    ".split("\n").map(&.strip).reject(&.empty?)
  end
end
