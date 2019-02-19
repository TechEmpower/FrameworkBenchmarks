require "./spec_helper"
require "../src/onyx/logger"

describe Onyx do
  describe ".logger" do
    it do
      Onyx.logger.should be_a(Logger)
    end
  end
end
