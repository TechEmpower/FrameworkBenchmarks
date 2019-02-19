require "../../spec_helper"

describe Onyx::SQL::Repository::Logger::IO do
  it do
    io = IO::Memory.new
    logger = Onyx::SQL::Repository::Logger::IO.new(io, false)
    logger.wrap("foo") { "bar" }.should eq "bar"
    io.to_s.should match %r{foo\n.+s\n}
  end
end
