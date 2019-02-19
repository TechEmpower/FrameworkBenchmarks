require "../../spec_helper"

describe Onyx::SQL::Repository::Logger::Dummy do
  it do
    logger = Onyx::SQL::Repository::Logger::Dummy.new
    logger.wrap("foo") { "bar" }.should eq "bar"
  end
end
