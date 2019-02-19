require "../../spec_helper"

describe Onyx::SQL::Repository::Logger::Standard do
  it do
    io = IO::Memory.new
    standard_logger = ::Logger.new(io, ::Logger::Severity::DEBUG)
    logger = Onyx::SQL::Repository::Logger::Standard.new(standard_logger, ::Logger::Severity::INFO, false)
    logger.wrap("foo") { "bar" }.should eq "bar"
    io.to_s.should match %r{I, \[.+\]  INFO -- : foo\nI, \[.+\]  INFO -- : .+s\n}
  end
end
