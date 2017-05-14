require "./spec_helper"
require "../../src/models/fortune.cr"

describe Fortune do
  Spec.before_each do
    Fortune.clear
  end
end
